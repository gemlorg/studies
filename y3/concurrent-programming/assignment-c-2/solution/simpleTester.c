#include <stdio.h>
#include <assert.h>
// #include <threads.h>

#include "BLQueue.h"
#include "HazardPointer.h"
#include "LLQueue.h"
#include "SimpleQueue.h"
#include "RingsQueue.h"

// A structure holding function pointers to methods of some queue type.
struct QueueVTable {
    const char* name;
    void* (*new)(void);
    void (*push)(void* queue, Value item);
    Value (*pop)(void* queue);
    bool (*is_empty)(void* queue);
    void (*delete)(void* queue);
};
typedef struct QueueVTable QueueVTable;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wincompatible-pointer-types"

const QueueVTable queueVTables[] = {
    //{ "SimpleQueue", SimpleQueue_new, SimpleQueue_push, SimpleQueue_pop, SimpleQueue_is_empty, SimpleQueue_delete },
    { "LLQueue", LLQueue_new, LLQueue_push, LLQueue_pop, LLQueue_is_empty, LLQueue_delete },
    // { "BLQueue", BLQueue_new, BLQueue_push, BLQueue_pop, BLQueue_is_empty, BLQueue_delete }
    //{ "RingsQueue", RingsQueue_new, RingsQueue_push, RingsQueue_pop, RingsQueue_is_empty, RingsQueue_delete }
};

#pragma GCC diagnostic pop

void basic_test(QueueVTable Q)
{
    HazardPointer_register(0, 1);
    void* queue = Q.new();
    // assert(Q.is_empty(queue));

    Q.push(queue, 1);
    // // assert(!Q.is_empty(queue));
    Q.push(queue, 2);
    Q.push(queue, 3);
    Value a = Q.pop(queue);
    //  assert(!Q.is_empty(queue));
    Value b = Q.pop(queue);
    //  assert(!Q.is_empty(queue));
    Value c;
    
    // printf("%lu %lu %lu\n", a, b, c);
    for(int i = 1; i < 10500; i++) {
        printf("\n");
        printf("val: %d\n", i);
        Q.push(queue, i);
        c = Q.pop(queue);
        printf("value: %lu\n", c);
        printf("\n");
    }
    // assert(Q.is_empty(queue));
    printf("%lu\n", a);

    Q.delete(queue);
}

int main(void)
{
    printf("Hello, World!\n");

    for (int i = 0; i < sizeof(queueVTables) / sizeof(QueueVTable); ++i) {
        QueueVTable Q = queueVTables[i];
        printf("Queue type: %s\n", Q.name);
        basic_test(Q);
    }

    return 0;
}
