#ifdef __linux__
#include <malloc.h>
#endif
#include <stdlib.h>
#include <pthread.h>
#include <stdatomic.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>


#include "RingsQueue.h"

// //can't edit .h files, so I have to define the function in each file
// //taken from lab files
static inline void syserr(const char* fmt, ...)
{
    va_list fmt_args;

    fprintf(stderr, "ERROR: ");

    va_start(fmt_args, fmt);
    vfprintf(stderr, fmt, fmt_args);
    va_end(fmt_args);
    fprintf(stderr, " (%d; %s)\n", errno, strerror(errno));
    exit(1);
}
#define ASSERT_SYS_OK(expr)                                                    \
  do {                                                                         \
    if ((expr) == -1)                                                          \
      syserr("system command failed: %s\n\tIn function %s() in %s line "       \
             "%d.\n\tErrno: ",                                                 \
             #expr, __func__, __FILE__, __LINE__);                             \
  } while (0)

struct RingsQueueNode;
typedef struct RingsQueueNode RingsQueueNode;
// atomowy wskaźnik next na następny węzeł w liście,
// bufor cykliczny w postaci tabeli RING_SIZE wartości typu Value,
// atomowy licznik push_idx wykonanych w tym węźle push-ów,
// atomowy licznik pop_idx wykonanych w tym węźle pop-ów.
struct RingsQueueNode {
    _Atomic(RingsQueueNode*) next;
    Value buffer[RING_SIZE]; 
    _Atomic(size_t) push_idx;
    _Atomic(size_t) pop_idx;
};

// TODO RingsQueueNode_new

struct RingsQueue {
    RingsQueueNode* head;
    RingsQueueNode* tail;
    pthread_mutex_t pop_mtx;
    pthread_mutex_t push_mtx;
};

RingsQueueNode* RingsQueueNode_new(void)
{
    RingsQueueNode* node = (RingsQueueNode*)malloc(sizeof(RingsQueueNode));
    
    atomic_init(&node->next, NULL);
    atomic_init(&node->pop_idx, 0);
    atomic_init(&node->push_idx, 0);
    return node;
}

void insertValue(RingsQueueNode* node, Value item)
{   
    node->buffer[atomic_load(&node->push_idx) % RING_SIZE] = item;
    size_t val = atomic_load(&node->push_idx)+1;
    atomic_store(&node->push_idx, val);
}
Value removeValue(RingsQueueNode* node)
{   
    Value item = node->buffer[node->pop_idx % RING_SIZE];
    size_t val = atomic_load(&node->pop_idx)+1;
    atomic_store(&node->pop_idx, val);
    return item;
    
}
bool isNodeFull(RingsQueueNode* node)
{
    return atomic_load(&node->push_idx) == atomic_load(&node->pop_idx) + RING_SIZE;
}
bool isNodeEmpty(RingsQueueNode* node)
{
    return atomic_load(&node->push_idx)  == atomic_load(&node->pop_idx);
}

RingsQueue* RingsQueue_new(void)
{
    RingsQueue* queue = (RingsQueue*)malloc(sizeof(RingsQueue));
    RingsQueueNode* new = RingsQueueNode_new();
    queue->head = new;
    queue->tail = new;
    ASSERT_SYS_OK(pthread_mutex_init(&queue->pop_mtx, NULL));
    ASSERT_SYS_OK(pthread_mutex_init(&queue->push_mtx, NULL));
    return queue;
}

void RingsQueue_delete(RingsQueue* queue)
{
    if(queue == NULL) {
        return;
    }
    ASSERT_SYS_OK(pthread_mutex_destroy(&queue->pop_mtx));
    ASSERT_SYS_OK(pthread_mutex_destroy(&queue->push_mtx));
    RingsQueueNode* node = queue->head;
    while(node != NULL) {
        RingsQueueNode* next = atomic_load(&node->next);
        free(node);
        node = next;
    }
    free(queue);
}

void RingsQueue_push(RingsQueue* queue, Value item)
{
    if(queue == NULL) {
        return;
    }
    ASSERT_SYS_OK(pthread_mutex_lock(&queue->push_mtx));
    if(isNodeFull(queue->tail)) {
        RingsQueueNode* new = RingsQueueNode_new();
        atomic_store(&queue->tail->next, new);
        queue->tail = new;
    } 
    insertValue(queue->tail, item);
    
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->push_mtx));

}

Value RingsQueue_pop(RingsQueue* queue)
{
    if(queue == NULL) {
        return EMPTY_VALUE;
    }
    RingsQueueNode* node;
    RingsQueueNode* next;
    Value item;

    ASSERT_SYS_OK(pthread_mutex_lock(&queue->pop_mtx));
    node = queue->head;
    next = atomic_load(&node->next);
    if(isNodeEmpty(node)) {
        if(next == NULL) {
            ASSERT_SYS_OK(pthread_mutex_unlock(&queue->pop_mtx));
            return EMPTY_VALUE;
        } 
        queue->head = next;
        free(node);
    }
    item = removeValue(queue->head);
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->pop_mtx));
    return item; 
}

bool RingsQueue_is_empty(RingsQueue* queue)
{
    if(queue == NULL) {
        return true;
    }
    bool result;
    ASSERT_SYS_OK(pthread_mutex_lock(&queue->pop_mtx));
    result = isNodeEmpty(queue->head) && atomic_load(&queue->head->next) == NULL;
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->pop_mtx));
    return result;
}
