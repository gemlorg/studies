#ifdef __linux__
#include <malloc.h>
#endif
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "BLQueue.h"
#include "HazardPointer.h"

struct BLNode;
typedef struct BLNode BLNode;
typedef _Atomic(BLNode*) AtomicBLNodePtr;

// Struktura BLQueue składa się z:

// listy jednokierunkowej węzłów, gdzie węzeł zawiera:
// atomowy wskaźnik next na następny węzeł w liście,
// bufor z BUFFER_SIZE atomowych wartości typu Value,
// atomowy indeks push_idx następnego miejsca w buforze do wypełnienia przez push (rośnie przy każdej próbie push),
// atomowy indeks pop_idx następnego miejsca w buforze do opróżnienia przez pop (rośnie przy każdej próbie pop);
// atomowego wskaźnika head na pierwszy węzeł w liście;
// atomowego wskaźnika tail na ostatni węzeł w liście;
// struktury HazardPointer (patrz niżej).

struct BLNode {
    AtomicBLNodePtr next;
    _Atomic(Value) buffer[BUFFER_SIZE];
    _Atomic(size_t) push_idx;
    _Atomic(size_t) pop_idx;
};

BLNode* BLNode_new(void)
{
    BLNode* node = (BLNode*)malloc(sizeof(BLNode));
    atomic_init(&node->next, NULL);
    for (size_t i = 0; i < BUFFER_SIZE; ++i) {
        atomic_init(&node->buffer[i], EMPTY_VALUE);
    }
    atomic_init(&node->push_idx, 0);
    atomic_init(&node->pop_idx, 0);
    return node;
}

struct BLQueue {
    AtomicBLNodePtr head;
    AtomicBLNodePtr tail;
    HazardPointer hp;
};

BLQueue* BLQueue_new(void)
{
    BLQueue* queue = (BLQueue*)malloc(sizeof(BLQueue));
    BLNode* new = BLNode_new();
    queue->head = new;
    queue->tail = new;
    // HazardPointer_register(&queue->hp, 1);
    return queue;
}

void BLQueue_delete(BLQueue* queue)
{
    // TODO
    free(queue);
}

// Push powinien działać w pętli próbując wykonać następujące kroki:

// Odczytujemy wskaźnik na ostatni węzeł kolejki.
// Pobieramy i inkrementujemy z tego węzła indeks miejsca w buforze do wypełnienia przez push (nikt inny nie będzie już próbował push-ować w to miejsce).
// 3a. Jeśli indeks jest mniejszy niż rozmiar bufora, próbujemy wstawić element w to miejsce bufora.
// Jeśli inny wątek zdążył zmienić to miejsce (inny wątek wykonujący pop mógł je zmienić na TAKEN_VALUE), to próbujemy wszystko od nowa.
// A jeśli nam udało się wstawić element, wychodzimy z funkcji.
// 3b. Jeśli zaś indeks jest większy-równy niż rozmiar bufora, to znaczy, że bufor jest pełny i będziemy musieli utworzyć lub przejść do następnego węzła. 
//W tym celu najpierw sprawdzamy, czy został już utworzony następny węzeł.
// 4a. Jeśli tak, to upewniamy się że wskaźnik na ostatni węzeł w kolejce się zmienił i próbujemy wszystko od nowa.
// 4b. Jeśli nie, to tworzymy nowy węzeł, od razu z naszym jednym elementem w buforze. Próbujemy wskaźnik do nowego węzła wstawić jako następnik.
// Jeśli się nie udało (inny wątek zdążył już przedłużyć listę), to usuwamy nasz węzeł i próbujemy wszystko od nowa.
// Jeśli się udało, to aktualizujemy wskaźnik na ostatni węzeł w kolejce na nasz nowy węzeł

void BLQueue_push(BLQueue* queue, Value item)
{
    while(1) {
        BLNode* tail = atomic_load(&queue->tail);
        size_t idx = atomic_fetch_add(&tail->push_idx, 1);
        if (idx < BUFFER_SIZE) {
            Value initial = EMPTY_VALUE;
            if (atomic_compare_exchange_strong(&tail->buffer[idx], &initial, item)) {return;};
        } else {
            BLNode* next = atomic_load(&tail->next);
            if (next != NULL) {
                atomic_compare_exchange_strong(&queue->tail, &tail, next);
            } else {
                if(atomic_load(&tail->next) != NULL){ atomic_compare_exchange_strong(&queue->tail, &tail,tail->next);continue;}
                BLNode* new = BLNode_new();
                BLNode* next = NULL;
                // new->buffer[0] = item;
                // new->push_idx = 1;
                atomic_store(&new->buffer[0], item);
                atomic_store(&new->push_idx, 1);

                if (atomic_compare_exchange_strong(&tail->next,&next, new)) {
                    atomic_compare_exchange_strong(&queue->tail, &tail, new);
                    return;
                } else {
                    free(new);
                }
            }
        }
    
    }
}

// Pop powinien działać w pętli próbując wykonać następujące kroki:

// 1. Odczytujemy wskaźnik na pierwszy węzeł kolejki.
// 2. Pobieramy i inkrementujemy z tego węzła indeks miejsca w buforze do odczytania przez pop (nikt inny nie będzie już próbował pop-ować z tego miejsca).
// 3a. Jeśli indeks jest mniejszy niż rozmiar bufora, odczytujemy element z tego miejsca bufora i podmieniamy na TAKEN_VALUE.
// Jeśli pobraliśmy EMPTY_VALUE, to próbujemy wszystko od nowa.
// Jeśli pobraliśmy inny element, wychodzimy z funkcji.
// 3b. Jeśli zaś indeks jest większy-równy niż rozmiar bufora, to znaczy, że bufor jest całkowicie opróżniony i będziemy musieli przejść do następnego węzła. 
// W tym celu najpierw sprawdzamy, czy został już utworzony następny węzeł.
// 4a. Jeśli nie, to kolejka jest pusta, wychodzimy z funkcji.
// 4b. Jeśli tak, to upewniamy się że wskaźnik na pierwszy węzeł w kolejce się zmienił i próbujemy wszystko od nowa.
Value BLQueue_pop(BLQueue* queue)
{
    while(1) {
        BLNode* head = atomic_load(&queue->head);
        size_t idx = atomic_fetch_add(&head->pop_idx, 1);
        if (idx < BUFFER_SIZE) {
            // printf("looking for value, it's %lu\n", head->buffer[idx]);
            Value value = atomic_load(&head->buffer[idx]);
            if (value != EMPTY_VALUE && atomic_compare_exchange_strong(&head->buffer[idx], &value, TAKEN_VALUE)) return value;
        } else {
            BLNode* next = atomic_load(&head->next);
            // printf("looking for next block, it's %p\n", next);
            if (next == NULL) return EMPTY_VALUE;
            if(atomic_compare_exchange_strong(&queue->head, &head, next)) {
                HazardPointer_retire(&queue->hp, head);
            };
        }
    }
}

bool BLQueue_is_empty(BLQueue* queue)
{
    return true; // TODO
}
