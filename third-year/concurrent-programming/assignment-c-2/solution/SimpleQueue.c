#include <malloc.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdatomic.h>
#include <errno.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "SimpleQueue.h"

//can't edit .h files, so I have to define the function in each file
//taken from lab files
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



struct SimpleQueueNode;
typedef struct SimpleQueueNode SimpleQueueNode;

struct SimpleQueueNode {
    _Atomic(SimpleQueueNode*) next;
    Value item;
};

SimpleQueueNode* SimpleQueueNode_new(Value item)
{
    SimpleQueueNode* node = (SimpleQueueNode*)malloc(sizeof(SimpleQueueNode));
    atomic_init(&node->next, NULL);
    node->item = item;
    return node;
}

struct SimpleQueue {
    SimpleQueueNode* head;
    SimpleQueueNode* tail;
    pthread_mutex_t head_mtx;
    pthread_mutex_t tail_mtx;
};

SimpleQueue* SimpleQueue_new(void)
{
    SimpleQueue* queue = (SimpleQueue*)malloc(sizeof(SimpleQueue));
    SimpleQueueNode* new = SimpleQueueNode_new(EMPTY_VALUE);
    queue->head = new;
    queue->tail = new;
    ASSERT_SYS_OK(pthread_mutex_init(&queue->head_mtx, NULL));
    ASSERT_SYS_OK(pthread_mutex_init(&queue->tail_mtx, NULL));
    return queue;
}

void SimpleQueue_delete(SimpleQueue* queue)
{
    if(queue == NULL) {
        return;
    }
    ASSERT_SYS_OK(pthread_mutex_destroy(&queue->head_mtx));
    ASSERT_SYS_OK(pthread_mutex_destroy(&queue->tail_mtx));
    SimpleQueueNode* node = queue->head;
    while(node != NULL) {
        SimpleQueueNode* next = atomic_load(&node->next);
        free(node);
        node = next;
    }
    free(queue);
}

void SimpleQueue_push(SimpleQueue* queue, Value item)
{
    if(queue == NULL) {
        return;
    }
    SimpleQueueNode* new = SimpleQueueNode_new(item);
    ASSERT_SYS_OK(pthread_mutex_lock(&queue->tail_mtx));
    atomic_store(&queue->tail->next, new);
    queue->tail = new;
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->tail_mtx));
}

Value SimpleQueue_pop(SimpleQueue* queue)
{
    if(queue == NULL) {
        return EMPTY_VALUE;
    }
    SimpleQueueNode* node;
    SimpleQueueNode* next;
    Value item;

    ASSERT_SYS_OK(pthread_mutex_lock(&queue->head_mtx));
    node = queue->head;
    next = atomic_load(&node->next);
    if(next == NULL) {
        ASSERT_SYS_OK(pthread_mutex_unlock(&queue->head_mtx));
        return EMPTY_VALUE;
    }
    queue->head = next;
    item = next->item;
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->head_mtx));
    free(node);
    return item; 
}

bool SimpleQueue_is_empty(SimpleQueue* queue)
{
    if(queue == NULL) {
        return true;
    }
    bool result;
    ASSERT_SYS_OK(pthread_mutex_lock(&queue->head_mtx));
    // ASSERT_SYS_OK(pthread_mutex_lock(&queue->tail_mtx));
    result = (queue->head == queue->tail);
    // ASSERT_SYS_OK(pthread_mutex_unlock(&queue->tail_mtx));
    ASSERT_SYS_OK(pthread_mutex_unlock(&queue->head_mtx));
    return result;
}
