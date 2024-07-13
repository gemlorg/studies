#ifdef __linux__
#include <malloc.h>
#endif
#include <stdlib.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <assert.h>

#include "HazardPointer.h"
#include "LLQueue.h"

struct LLNode;
typedef struct LLNode LLNode;
typedef _Atomic(LLNode*) AtomicLLNodePtr;

struct LLNode {
    AtomicLLNodePtr next;
    _Atomic(Value) value;
};

LLNode* LLNode_new(Value item)
{
    LLNode* node = (LLNode*)malloc(sizeof(LLNode));
    atomic_init(&node->next, NULL);
    atomic_store(&node->value, item);
    return node;
}

struct LLQueue {
    AtomicLLNodePtr head;
    AtomicLLNodePtr tail;
    HazardPointer hp;
};

LLQueue* LLQueue_new(void)
{

    LLQueue* queue = (LLQueue*)malloc(sizeof(LLQueue));
    LLNode* new = LLNode_new(EMPTY_VALUE);
    atomic_store(&queue->head, new);
    atomic_store(&queue->tail, new);
    HazardPointer_initialize(&queue->hp);
    return queue;
}

void LLQueue_delete(LLQueue* queue)
{
    if(queue == NULL) return;
    HazardPointer_finalize(&queue->hp);
    LLNode* node = atomic_load(&queue->head);
    while(node != NULL) {
        LLNode* next = atomic_load(&node->next);
        free(node);
        node = next;
    }
    free(queue);
}
// Push powinien działać w pętli próbując wykonać następujące kroki:

// Odczytujemy wskaźnik na ostatni węzeł kolejki (my tzn. wątek wykonujący push).
// Zamieniamy następnika na nowy węzeł z naszym elementem.
// 3a. Jeśli się udało, aktualizujemy wskaźnik na ostatni węzeł w kolejce na nasz nowy węzeł i wychodzimy z funkcji.
// 3b. Jeśli się nie udało (inny wątek zdążył już przedłużyć listę), to próbujemy wszystko od nowa, upewniając się że ostatni węzeł został zaktualizowany.

void LLQueue_push(LLQueue* queue, Value item)
{
    if(queue == NULL) return;

    LLNode* new = LLNode_new(item);
    while(1) {

        LLNode* tail = HazardPointer_protect(&queue->hp, (_Atomic(void*)*)&queue->tail);
        if (tail != atomic_load(&queue->tail)) continue;
        LLNode* next = NULL;
        if (atomic_load(&tail->next) == NULL) {
            if (atomic_compare_exchange_strong(&tail->next, &next, new)) {
                 atomic_compare_exchange_strong(&queue->tail, &tail, new);
                 HazardPointer_clear(&queue->hp);
                return;
            }
        } else {
            atomic_compare_exchange_strong(&queue->tail, &tail, tail->next);
        }
        HazardPointer_clear(&queue->hp);
    }
}


    // while(1) {
    //     LLNode* head = atomic_load(&queue->head);
    //     LLNode* tail = atomic_load(&queue->tail);
    //     LLNode* first = atomic_load(&head->next);
    //     if (head == atomic_load(&queue->head)) {
    //         if (head == tail) {
    //             if (first == NULL) {
    //                 return EMPTY_VALUE;
    //             }
    //             atomic_compare_exchange_strong(&queue->tail, &tail, first);
    //         } else {
    //             Value value = first->value;
    //             if (atomic_compare_exchange_strong(&queue->head, &head, first)) {
    //                 HazardPointer_retire(&queue->hp, head);
    //                 return value;
    //             }
    //         }
    //     }
    // }

// Pop powinien działać w pętli próbując wykonać następujące kroki:
// Odczytujemy wskaźnik na pierwszy węzeł kolejki.
// Odczytujemy wartość z tego węzła i zamieniamy ją na EMPTY_VALUE.
// 3a. Jeśli odczytana wartość była inna niż EMPTY_VALUE, to aktualizujemy wskaźnik na pierwszy węzeł (jeśli potrzeba) i zwracamy wynik.
// 3b. Jeśli odczytana wartość była EMPTY_VALUE, to sprawdzamy czy kolejka jest pusta.
// Jeśli tak, zwracamy EMPTY_VALUE, a jeśli nie, to próbujemy wszystko od nowa, upewniając się że pierwszy węzeł został zaktualizowany.

Value LLQueue_pop(LLQueue* queue)
{

    if(queue == NULL) return EMPTY_VALUE;
    while(1) {
        
        LLNode* head = HazardPointer_protect(&queue->hp, (_Atomic(void*)*)&queue->head);

        if(head != atomic_load(&queue->head)) continue;
        Value val = atomic_exchange(&head->value, EMPTY_VALUE);
        LLNode* next = HazardPointer_protect(&queue->hp, (_Atomic(void*)*)&head->next);

        if(val != EMPTY_VALUE) {
            if(next != NULL) {
                if(atomic_compare_exchange_strong(&queue->head, &head, next)){}
                    // HazardPointer_retire(&queue->hp, head);
            }
            HazardPointer_clear(&queue->hp);
            return val;
        } else if (next == NULL) {
            HazardPointer_clear(&queue->hp);
            return EMPTY_VALUE;
        } else {
            if(atomic_compare_exchange_strong(&queue->head, &head, next)){}
                // HazardPointer_retire(&queue->hp, head);
        }
    }
}

bool LLQueue_is_empty(LLQueue* queue)
{   
    LLNode* head;
    do {
    head = HazardPointer_protect(&queue->hp, (_Atomic(void*)*)&queue->head);
    } while (head != atomic_load(&queue->head));
    bool result = atomic_load(&head->next) == NULL && atomic_load(&head->value) == EMPTY_VALUE;
    HazardPointer_clear(&queue->hp);
    return result;
}
