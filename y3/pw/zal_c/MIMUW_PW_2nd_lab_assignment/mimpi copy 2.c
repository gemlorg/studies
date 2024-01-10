/**
 * This file is for implementation of MIMPI library.
 * */

#include "mimpi.h"
#include "channel.h"
#include "mimpi_common.h"
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "examples/mimpi_err.h"

#define byte_t uint8_t
#define MSG_SIZE (512 - 7 * sizeof(int))

typedef enum { MIMPI_SYNC_TAG1 = -1, MIMPI_SYNC_TAG2 = -2 } MIMPI_Tagcode;

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
  int id;
  int total_fractions;
  int fraction_count;
  // int source;
  int size;
  byte_t *data;
};
typedef struct message message;

struct message_node {
  message *msg;
  struct message_node *next;
};
typedef struct message_node message_node;

struct procinfo {
  bool deadlock_detection;
  int id;
  int n;
  int read_fd[MAX_TOTAL];
  int write_fd[MAX_TOTAL];
  int next_mid;

  bool has_finished[MAX_TOTAL];
  message_node *messages[MAX_TOTAL];

  pthread_attr_t attr;
  pthread_t threads[MAX_TOTAL];
  pthread_mutex_t recv_mutex[MAX_TOTAL];

  bool waiting_for_recv[MAX_TOTAL];
  int waited_tag;
  int waited_count;
  pthread_cond_t waiting_place;

} info;

bool equal_tags(int tag1, int tag2) {
  // maybe only tag2
  if (tag1 >= 0 && tag2 >= 0)
    return tag1 == tag2 || tag1 == MIMPI_ANY_TAG || tag2 == MIMPI_ANY_TAG;
  return tag1 == tag2;
}
bool ready_message(message *msg) {
  return msg->fraction_count == msg->total_fractions;
}

message *get_message(int source, int tag, int count) {
  message_node *current = info.messages[source];
  message_node *temp;

  if (info.messages[source] != NULL &&
      equal_tags(info.messages[source]->msg->tag, tag) &&
      info.messages[source]->msg->size == count &&
      ready_message(info.messages[source]->msg)) {
    message *elem = info.messages[source]->msg;
    temp = info.messages[source];
    info.messages[source] = info.messages[source]->next;
    free(temp);
    return elem;
  }
  
  while (current != NULL && current->next != NULL) {
    assert(current->next->msg != NULL);
    message *elem = current->next->msg;
    if ((equal_tags(elem->tag, tag)) && elem->size == count &&
        ready_message(elem)) {
      temp = current->next;
      current->next = current->next->next;
      free(temp);
      return elem;
    }
    current = current->next;
  }
  
  return NULL;
}

void print_messages(message_node *head, int source) {
  message_node *current = head;
  // printf("messages, source %d:\n", source);
  while (current != NULL) {
    // printf("tag: %d, size: %d\n", current->msg->tag, current->msg->size);
    current = current->next;
  }
}

message *create_message(fraction frac) {
  message *msg = malloc(sizeof(message));
  msg->tag = frac.tag;
  msg->id = frac.id;
  msg->total_fractions = frac.total_fractions;
  msg->fraction_count = 1;
  msg->size = frac.total_size;
  msg->data = malloc(frac.total_size);
  int frac_len = frac.fraction_id == frac.total_fractions - 1
                     ? frac.total_size - frac.fraction_id * MSG_SIZE
                     : MSG_SIZE;
  memcpy(msg->data + frac.fraction_id * MSG_SIZE, frac.data, frac_len);
  return msg;
}

message_node *create_node(message *msg) {
  message_node *node = malloc(sizeof(message_node));
  node->msg = msg;
  node->next = NULL;
  return node;
}

void add_fraction_to_message(message *msg, fraction frac) {
  int frac_len = frac.fraction_id == frac.total_fractions - 1
                     ? frac.total_size - frac.fraction_id * MSG_SIZE
                     : MSG_SIZE;
  memcpy(msg->data + frac.fraction_id * MSG_SIZE, frac.data, frac_len);
  msg->fraction_count++;
}

bool add_fraction(fraction frac, int source) {
  if (info.messages[source] == NULL) {
    info.messages[source] = create_node(create_message(frac));
    return frac.total_fractions == 1;
  }
  message_node *current = info.messages[source];

  while (current != NULL) {
    if (current->msg->id == frac.id) {
      add_fraction_to_message(current->msg, frac);
      return current->msg->fraction_count == current->msg->total_fractions;
    }
    if (current->next == NULL) {
      break;
    }
    current = current->next;
  }

  current->next = create_node(create_message(frac));
  return frac.total_fractions == 1;
}

void *worker(void *data) {
  int id = *((int *)data);
  assert(id != info.id);
  free(data);

  // printf("thread %d on process %d started\n", id, info.id);
  fraction msg;

  while (true) {
    memset(msg.data, 0, MSG_SIZE);
    if (chrecv(info.read_fd[id], &msg, sizeof(msg)) <= 0) {
      // printf("proc %d channel %d closed\n", info.id, id);
      // printf("thread %d on process %d finished\n", id, info.id);
      ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));
      info.has_finished[id] = true;
      if (info.waiting_for_recv[id]) {
        info.waiting_for_recv[id] = false;
        pthread_cond_signal(&info.waiting_place);
      }
      ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
      return NULL;
    }

    ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));
    bool end_of_message = add_fraction(
        msg, id); // check if has all parts and if does put it to ready messages
    // if(end_of_message) printf("process %d got message from %d\n", info.id, id);
    if (end_of_message && info.waiting_for_recv[id] &&
        equal_tags(msg.tag, info.waited_tag) &&
        info.waited_count == msg.total_size) {
        
      info.waiting_for_recv[id] = false;
      // printf("added message process %d\n", info.id);

      pthread_cond_signal(&info.waiting_place);
    }
    // check if waiting for this message
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
  }

  return NULL;
}

// maybe check that enters only once
// maybe check in each function that init was called

void MIMPI_Init(bool enable_deadlock_detection) {
  // printf("Process %d second phase of init\n", info.id);
  channels_init();
  info.deadlock_detection = enable_deadlock_detection;
  info.next_mid = 0;
  info.id = atoi(getenv(MIMPI_ID));
  info.n = atoi(getenv(MIMPI_TOTAL));
  pthread_cond_init(&info.waiting_place, NULL);

  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    ASSERT_ZERO(pthread_mutex_init(&info.recv_mutex[i], NULL));
    info.has_finished[i] = false;
    info.messages[i] = NULL;
    info.has_finished[i] = false;
    info.waiting_for_recv[i] = false;
  }

  // init pipes
  char str1[ENOUGH_SPACE];
  char str2[ENOUGH_SPACE];
  // printf("finished MIMPI_Init\n");
  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    MIMPI_READ_FD(i, info.id, str1);
    MIMPI_WRITE_FD(info.id, i, str2);
    // printf("id %d: str1: %s, str2: %s\n",id, str1, str2);
    info.read_fd[i] = atoi(getenv(str1));
    info.write_fd[i] = atoi(getenv(str2));
    // printf("read_fd[%d]: %d, write_fd[%d]: %d\n", i, getenv, i, write_fd[i]);
    // printf("read_fd[%d]: %s, write_fd[%d]: %s\n", i, getenv(str1), i,
    // getenv(str2));
  }

  // init threads
  int detach_state = PTHREAD_CREATE_JOINABLE;

  // printf("Process %d is creating threads.\n", getpid());

  // Create thread attributes.

  ASSERT_ZERO(pthread_attr_init(&info.attr));
  ASSERT_ZERO(pthread_attr_setdetachstate(&info.attr, detach_state));

  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    int *worker_arg = malloc(sizeof(int));
    *worker_arg = i;
    ASSERT_ZERO(
        pthread_create(&info.threads[i], &info.attr, worker, worker_arg));
  }
  // sleep(1);
}

void MIMPI_Finalize() {
  // wait for threads to init!!!
  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    ASSERT_SYS_OK(close(info.read_fd[i]));
    ASSERT_SYS_OK(close(info.write_fd[i]));
  }
  // printf("Process %d is finalizing.\n", info.id);
  for(int i = 0; i < info.n; i++) {
    if(i == info.id)
      continue;
    ASSERT_SYS_OK(pthread_join(info.threads[i], NULL));
  }
  ASSERT_ZERO(pthread_attr_destroy(&info.attr));
  channels_finalize();
}

int MIMPI_World_size() { return info.n; }

int MIMPI_World_rank() { return info.id; }

MIMPI_Retcode MIMPI_Send(void const *data, int count, int destination,
                         int tag) {
  // checks
  if(destination >= info.n) {
  return MIMPI_ERROR_NO_SUCH_RANK;
}

if(destination == info.id) {
  return MIMPI_ERROR_ATTEMPTED_SELF_OP;
}
// if(info.has_finished[destination]) { 
//   return MIMPI_ERROR_REMOTE_FINISHED;
// }


  int num_fractions = (count + MSG_SIZE - 1) / MSG_SIZE;
  fraction msg;
  msg.tag = tag;
  msg.source = info.id;
  info.next_mid++;
  msg.id = info.next_mid++;
  msg.total_size = count;
  msg.total_fractions = num_fractions;
  for (int fraction_id = 0; fraction_id < num_fractions; fraction_id++) {
    msg.fraction_id = fraction_id;
    int msg_len = fraction_id == num_fractions - 1
                      ? count - fraction_id * MSG_SIZE
                      : MSG_SIZE;
    memset(msg.data, 0, MSG_SIZE);
    memcpy(msg.data, data + fraction_id * MSG_SIZE, msg_len);
    // printf("sending fraction: id: %d, tag: %d, source: %d, total_size: %d,
    // total_fractions: %d, fraction_id: %d\n", msg.id, msg.tag, msg.source,
    // msg.total_size, msg.total_fractions, msg.fraction_id); printf("size of
    // message %d\n", sizeof(msg));

    //at this point we have checked that destination is active, so if we get EPIPE it's UB
    if(chsend(info.write_fd[destination], &msg, sizeof(msg) < 0)) {
      ASSERT_ZERO(errno != EPIPE);
      return MIMPI_ERROR_REMOTE_FINISHED;

    }

  }
  return MIMPI_SUCCESS;
}

MIMPI_Retcode MIMPI_Recv(void *data, int count, int source, int tag) {
  message *msg;

if(source >= info.n) {
  return MIMPI_ERROR_NO_SUCH_RANK;
}

if(source == info.id) {
  return MIMPI_ERROR_ATTEMPTED_SELF_OP;
}

  ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[source]));

  if ((msg = get_message(source, tag, count)) != NULL) {
    memcpy(data, msg->data, count);
    free(msg->data);
    free(msg);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_SUCCESS;
  } else if (info.has_finished[source]) {
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_ERROR_REMOTE_FINISHED;
  }
  // printf("waiting for message\n");
  info.waiting_for_recv[source] = true;
  info.waited_tag = tag;
  info.waited_count = count;
  pthread_cond_wait(&info.waiting_place, &info.recv_mutex[source]);
  // printf("wait ended\n");
  if ((msg = get_message(source, tag, count)) != NULL) {
    memcpy(data, msg->data, count);
    free(msg->data);
    free(msg);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_SUCCESS;
  }
  // printf("process %d didn't get message from %d\n", info.id, source);

  ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
  return MIMPI_ERROR_REMOTE_FINISHED;
}


MIMPI_Retcode MIMPI_Barrier() {
  int d = 0;
  return MIMPI_Bcast(&d, sizeof(int), 0);
}

// root has index 1
int treeid(int root, int id) { return (info.n + id - root) % info.n + 1; }
int realid(int root, int tree_id) { return (tree_id - 1 + root) % info.n; }
int son1(int id) { return 2 * id; }
int son2(int id) { return 2 * id + 1; }
int father(int id) { return id / 2; }

MIMPI_Retcode MIMPI_Bcast(void *data, int count, int root) {
  int tree_id = treeid(root, info.id);
  int s1 = son1(tree_id);
  int s2 = son2(tree_id);
  int d = 0;
  int i;

  if (s1 <= info.n) {
    if((i = MIMPI_Recv(&d, 1, realid(root, s1), MIMPI_SYNC_TAG2)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (s2 <= info.n) {
    if((i = MIMPI_Recv(&d, 1, realid(root, s2), MIMPI_SYNC_TAG2)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (tree_id != 1) {
    if((i = MIMPI_Send(&d, 1, realid(root, father(tree_id)), MIMPI_SYNC_TAG2)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  }

  if (tree_id != 1) {
    if((i = MIMPI_Recv(data, count, realid(root, father(tree_id)), MIMPI_SYNC_TAG1)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  } 
  if (s1 <= info.n) {

    if((i = MIMPI_Send(data, count, realid(root, s1), MIMPI_SYNC_TAG1)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (s2 <= info.n) {
    if((i = MIMPI_Send(data, count, realid(root, s2), MIMPI_SYNC_TAG1)) != MIMPI_SUCCESS) 
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  

  assert(tree_id == treeid(root, info.id) && s1 == son1(tree_id) &&
         s2 == son2(tree_id));
  // printf("process %d with tree_id %d finished bcast\n", info.id, tree_id);

  return MIMPI_SUCCESS;
}

void reduce(byte_t *data1, byte_t *data2, int count, MIMPI_Op op) {
  switch (op) {
  case MIMPI_SUM:
    for (int i = 0; i < count; i++) {
      data1[i] += data2[i];
    }
    break;
  case MIMPI_PROD:
    for (int i = 0; i < count; i++) {
      data1[i] *= data2[i];
    }
    break;
  case MIMPI_MAX:
    for (int i = 0; i < count; i++) {
      data1[i] = data1[i] > data2[i] ? data1[i] : data2[i];
    }
    break;
  case MIMPI_MIN:
    for (int i = 0; i < count; i++) {
      data1[i] = data1[i] < data2[i] ? data1[i] : data2[i];
    }
    break;
  }
}



MIMPI_Retcode MIMPI_Reduce(void const *send_data, void *recv_data, int count,
                           MIMPI_Op op, int root) {

  int tree_id = treeid(root, info.id);
  int s1 = son1(tree_id);
  int s2 = son2(tree_id);
  byte_t *my_data = malloc(count * sizeof(byte_t));
  memcpy(my_data, send_data, count);
  int d = 0;
  byte_t *d1 = malloc(count * sizeof(byte_t));
  byte_t *d2 = malloc(count * sizeof(byte_t));
  if (s1 <= info.n) {
    ASSERT_MIMPI_OK(MIMPI_Recv(d1, count, realid(root, s1), MIMPI_SYNC_TAG1));
    reduce(my_data, d1, count, op);
  }
  

  if (s2 <= info.n) {
    ASSERT_MIMPI_OK(MIMPI_Recv(d2, count, realid(root, s2), MIMPI_SYNC_TAG1));
    reduce(my_data, d2, count, op);
  }
  

  if (tree_id != 1) {
    ASSERT_SYS_OK(MIMPI_Send(my_data, count, realid(root, father(tree_id)), MIMPI_SYNC_TAG1));
    ASSERT_SYS_OK(MIMPI_Recv(&d, sizeof(int), realid(root, father(tree_id)), MIMPI_SYNC_TAG2));

  } else {
    memcpy(recv_data, my_data, count);
  }
  
  
  if (s1 <= info.n) {
    ASSERT_SYS_OK(MIMPI_Send(&d, sizeof(int), realid(root, s1), MIMPI_SYNC_TAG2));
  }
  if (s2 <= info.n) {
    ASSERT_SYS_OK(MIMPI_Send(&d, sizeof(int), realid(root, s2), MIMPI_SYNC_TAG2));
  }
  free(d1);
  free(d2);
  free(my_data);
  return MIMPI_SUCCESS;
}
