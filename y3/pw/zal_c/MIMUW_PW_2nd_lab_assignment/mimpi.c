/**
 * This file is for implementation of MIMPI library.
 * */

#include "channel.h"
#include "mimpi.h"
#include "mimpi_common.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>


#define byte_t uint8_t
#define MSG_SIZE (512 - 6 * sizeof(int))



struct message_fraction {
    int tag;
    int id;
    int source;
    int total_fractions;
    int fraction_id;
    int total_size;
    byte_t data[MSG_SIZE];
};
typedef struct message_fraction fraction;

struct message {
    int tag;
    // int id;
    // int source;
    int size;
    byte_t *data;
};
typedef struct message message;

struct  message_node {
    message *msg;
    struct message_node *next;
};
typedef struct message_node message_node;

struct  fraction_node {
    fraction *msg;
    struct fraction_node *next;
};
typedef struct fraction_node fraction_node;


struct procinfo {
    bool deadlock_detection;
    int id;
    int n;
    int read_fd[MAX_TOTAL];
    int write_fd[MAX_TOTAL];
    int next_mid ;

    bool has_finished[MAX_TOTAL];
    message_node *messages[MAX_TOTAL];
    fraction_node *fractions[MAX_TOTAL];



    pthread_attr_t attr;
    pthread_t threads[MAX_TOTAL];
    pthread_mutex_t recv_mutex[MAX_TOTAL];

    bool waiting_for_recv[MAX_TOTAL];
    int waited_tag;
    int waited_count;
    pthread_cond_t waiting_place;


} info;

bool equal_tags(int tag1, int tag2) {
    //maybe only tag2
    return tag1 == tag2 || tag1 == MIMPI_ANY_TAG || tag2 == MIMPI_ANY_TAG;
}

bool have_message(int source, int tag, int count) {
    message_node *current = info.messages[source];
    while(current != NULL) {
        if(equal_tags(current->msg->tag, tag) && current->msg->size == count) {
            return true;
        }
        current = current->next;
    }
    return false;
}

message *get_message(int source, int tag, int count) {
    message_node *current = info.messages[source];
    message_node *temp;
    //we know it exists.  
    while(current != NULL && current->next != NULL) {
        message *elem = current->next->msg;
        if((elem->tag == tag || tag == MIMPI_ANY_TAG) && elem->size == count) {
            temp = current->next;
            current->next = current->next->next;
            free(temp);
            return elem;
        }
        current = current->next;
    }
    if(info.messages[source] != NULL && (info.messages[source]->msg->tag == tag || tag == MIMPI_ANY_TAG) && info.messages[source]->msg->size == count) {
        message *elem = info.messages[source]->msg;
        temp = info.messages[source];
        info.messages[source] = info.messages[source]->next;
        free(temp);
        return elem;
    }
    return NULL;
}
void print_fractions(fraction_node *head, int source) {
    fraction_node *current = head;
    printf("fractions, source %d:\n", source);
    while(current != NULL) {
        printf("tag: %d, id: %d, source: %d, total_fractions: %d, fraction_id: %d, total_size: %d\n", 
        current->msg->tag, current->msg->id, current->msg->source, current->msg->total_fractions, current->msg->fraction_id, current->msg->total_size);
        current = current->next;
    }
    printf("end\n");
}

void fraction_put(int source, fraction frac) {
    fraction_node *new_node = malloc(sizeof(fraction_node));
    new_node->msg = malloc(sizeof(fraction));
    memcpy(new_node->msg, &frac, sizeof(fraction));
    new_node->next = NULL;
    // printf("fraction put\n");

    if(info.fractions[source] == NULL) {
        info.fractions[source] = new_node;
        // print_fractions(info.fractions[source], source);
        return;
    }
    


    fraction_node *last = info.fractions[source];
    while(last->next != NULL) {
        last = last->next;
    }
    last->next = new_node;
    
}
int count_fractions(int id, int source) {
    int count = 0;
    fraction_node *current = info.fractions[source];
    while(current != NULL) {
        if(current->msg->id == id) {
            count++;
        }
        current = current->next;
    }
    return count;
}
void print_messages(message_node *head, int source) {
    message_node *current = head;
    // printf("messages, source %d:\n", source);
    while(current != NULL) {
        // printf("tag: %d, size: %d\n", current->msg->tag, current->msg->size);
        current = current->next;
    }
}

message *compose_message(int id, int source, int total_size) {
    // printf("composing message\n");
    message *msg = malloc(sizeof(message));
    msg->size = total_size;
    msg->data = malloc(total_size);


    fraction_node *current = info.fractions[source];
    fraction_node *temp;
    // print_fractions(info.fractions[source], source);
    while(current != NULL && current->next != NULL) {
        fraction *elem = current->next->msg;
        if(elem->id == id) {
            msg->tag = elem->tag;
            
            memcpy(msg->data + elem->fraction_id * MSG_SIZE, 
            elem->data, elem->fraction_id == elem->total_fractions - 1 ? 
            elem->total_size -  elem->fraction_id * MSG_SIZE : 
            MSG_SIZE);

            temp = current->next;
            current->next = current->next->next;
            free(temp->msg); //or nah>
            free(temp);
        } else {
        current = current->next;
        }
    }
    
    if(info.fractions[source] != NULL && info.fractions[source]->msg->id == id ) {
        fraction* elem = info.fractions[source]->msg;
        msg->tag = elem->tag;
        memcpy(msg->data + elem->fraction_id * MSG_SIZE, 
            elem->data, elem->fraction_id == elem->total_fractions - 1 ? 
            elem->total_size -  elem->fraction_id * MSG_SIZE : 
            MSG_SIZE);
        free(elem);
        free(info.fractions[source]);
        info.fractions[source] = NULL;
    }
    return msg;
}

void add_message(int source, message *msg) {
    message_node *new_node = malloc(sizeof(message_node));
    new_node->msg = msg;
    new_node->next = NULL;

    if(info.messages[source] == NULL) {
        info.messages[source] = new_node;
        return;
    }

    message_node *last = info.messages[source];
    while(last->next != NULL) {
        last = last->next;
    }
    last->next = new_node;
}

//true if makes full message
bool add_fraction(fraction frac, int source) {
    fraction_put(source, frac);
    if(frac.total_fractions - count_fractions(frac.id, source) == 0) {
        message *message = compose_message(frac.id, source, frac.total_size);
        // printf("adding message:\n");
        // printf("tag: %d, size: %d, source: %d\n", message->tag, message->size, source);
        add_message(source, message);
            // printf("message is composed:\n");
            // print_messages(info.messages[source], source);
        return true;
    }   
    return false;
}


void *worker(void* data) {
    int id = *((int*)data);
    assert(id != info.id);
    free(data);

    // printf("thread %d on process %d started\n", id, info.id);
    fraction msg;

    while(true) {
        memset(&msg.data, 0, MSG_SIZE);
        if(chrecv(info.read_fd[id], &msg, sizeof(msg)) <= 0)  {
            // printf("proc %d channel %d closed\n", info.id, id);
            // printf("thread %d on process %d finished\n", id, info.id);
            ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));
            info.has_finished[id] = true;
            if(info.waiting_for_recv[id]) {
                info.waiting_for_recv[id] = false;
                pthread_cond_signal(&info.waiting_place);
            }
            ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
            return NULL;
        }

        // printf("got fraction\n");
        ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));
        bool end_of_message = add_fraction(msg, id); //check if has all parts and if does put it to ready messages
        if(end_of_message && info.waiting_for_recv[id] && equal_tags(msg.tag, info.waited_tag) && info.waited_count == msg.total_size) {
            info.waiting_for_recv[id] = false;
            // printf("added message process %d\n", info.id);
            
            pthread_cond_signal(&info.waiting_place);
        }
        //check if waiting for this message
        ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
    }

    return NULL;
}


//maybe check that enters only once
//maybe check in each function that init was called

void MIMPI_Init(bool enable_deadlock_detection) {
    // printf("Process %d second phase of init\n", info.id);
    channels_init();
    info.deadlock_detection = enable_deadlock_detection;
    info.next_mid = 0;
    info.id = atoi(getenv(MIMPI_ID));
    info.n = atoi(getenv(MIMPI_TOTAL));
    pthread_cond_init(&info.waiting_place, NULL);
    

    for(int i = 0; i < info.n; i++) {
        if (i == info.id) continue;
        ASSERT_ZERO(pthread_mutex_init(&info.recv_mutex[i], NULL));
        info.has_finished[i] = false;
        info.messages[i] = NULL;
        info.fractions[i] = NULL;
        info.has_finished[i] = false;
        info.waiting_for_recv[i] = false;
    }
    

    //init pipes
    char str1[ENOUGH_SPACE];
    char str2[ENOUGH_SPACE];
    // printf("finished MIMPI_Init\n");
    for(int i = 0; i < info.n; i++) {
        if (i == info.id) continue;
        MIMPI_READ_FD(i, info.id, str1);
        MIMPI_WRITE_FD(info.id, i, str2);
        // printf("id %d: str1: %s, str2: %s\n",id, str1, str2);
        info.read_fd[i] = atoi(getenv(str1));
        info.write_fd[i] = atoi(getenv(str2));
        // printf("read_fd[%d]: %d, write_fd[%d]: %d\n", i, getenv, i, write_fd[i]);
        // printf("read_fd[%d]: %s, write_fd[%d]: %s\n", i, getenv(str1), i, getenv(str2));
    }

    //init threads
    int detach_state = PTHREAD_CREATE_DETACHED;

    // printf("Process %d is creating threads.\n", getpid());

    // Create thread attributes.
    
    ASSERT_ZERO(pthread_attr_init(&info.attr));
    ASSERT_ZERO(pthread_attr_setdetachstate(&info.attr, detach_state));

    for (int i = 0; i < info.n; i++) {
        if (i == info.id) continue;
        int* worker_arg = malloc(sizeof(int));
        *worker_arg = i;
        ASSERT_ZERO(pthread_create(&info.threads[i], &info.attr, worker, worker_arg));
    }
    // sleep(1);    
}

void MIMPI_Finalize() {
    //wait for threads to init!!!
    for(int i = 0; i < info.n; i++) {
        if (i == info.id) continue;
        ASSERT_SYS_OK(close(info.read_fd[i]));
        ASSERT_SYS_OK(close(info.write_fd[i]));
    }
    
    ASSERT_ZERO(pthread_attr_destroy(&info.attr));
    channels_finalize();
}

int MIMPI_World_size() {
    return info.n;
}

int MIMPI_World_rank() {
    return info.id;
}

MIMPI_Retcode MIMPI_Send(
    void const *data,
    int count,
    int destination,
    int tag
) {
    //checks 

    int num_fractions = (count + MSG_SIZE - 1) / MSG_SIZE;
    fraction msg;
    msg.tag = tag;
    msg.source = info.id;
    msg.id = info.next_mid++;
    msg.total_size = count;
    msg.total_fractions = num_fractions;
    for(int fraction_id = 0; fraction_id < num_fractions; fraction_id++) {
        msg.fraction_id = fraction_id;
        memset(&msg.data, 0, MSG_SIZE);
        memcpy(msg.data, data + fraction_id * MSG_SIZE, fraction_id == num_fractions - 1 ? 
            count -  fraction_id * MSG_SIZE : 
            MSG_SIZE);
        chsend(info.write_fd[destination], &msg, sizeof(msg));
        //checks
    }
    return MIMPI_SUCCESS;
}



MIMPI_Retcode MIMPI_Recv(
    void *data,
    int count,
    int source,
    int tag
) {
    // printf("Entering MIMPI_Recv\n");
    ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[source]));
    if(!have_message(source, tag, count) && !info.has_finished[source]) {
        info.waiting_for_recv[source] = true;
        info.waited_tag = tag;
        info.waited_count = count;
        // printf("No message. waiting\n");
        pthread_cond_wait(&info.waiting_place, &info.recv_mutex[source]);
        // printf("Wait ended\n");
    }
    print_messages(info.messages[source], source);
    if(!have_message(source, tag, count)) {
        // printf("No message. returning\n");
        ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
        return MIMPI_ERROR_REMOTE_FINISHED;
    }
    // printf("got message:\n");
    message *msg = get_message(source, tag, count);
    
    // printf("tag: %d, size: %d, source: %d\n", msg->tag, msg->size, source);

    memcpy(data, msg->data, count);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return 0;
}

MIMPI_Retcode MIMPI_Barrier() {
    TODO
}

MIMPI_Retcode MIMPI_Bcast(
    void *data,
    int count,
    int root
) {
    TODO
}

MIMPI_Retcode MIMPI_Reduce(
    void const *send_data,
    void *recv_data,
    int count,
    MIMPI_Op op,
    int root
) {
    TODO
}