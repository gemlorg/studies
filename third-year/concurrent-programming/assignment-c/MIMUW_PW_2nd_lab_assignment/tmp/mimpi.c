/**
 * This file is for implementation of MIMPI library.
 * */

#include "mimpi.h"
#include "channel.h"
#include "examples/mimpi_err.h"
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

#define byte_t uint8_t
#define MSG_SIZE (512 - 7 * sizeof(int))

typedef enum {
  MIMPI_SYNC_TAG1 = -1,
  MIMPI_SYNC_TAG2 = -2,
  MIMPI_SYNC_DEADLOCK_TAG = -3,
  MIMPI_INIT_TAG = -4
} MIMPI_Tagcode;

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
  int size;
  byte_t *data;
};
typedef struct message message;

struct message_info_node {
  int tag;
  int count;
  struct message_info_node *next;
};
typedef struct message_info_node message_info_node;

struct message_node {
  message *msg;
  struct message_node *next;
};
typedef struct message_node message_node;

struct procinfo {
  bool deadlock_detection;
  int id;
  int n;
  int read_fd[MIMPI_MAX_TOTAL];
  int write_fd[MIMPI_MAX_TOTAL];
  int next_mid;

  bool has_finished[MIMPI_MAX_TOTAL];
  message_node *messages[MIMPI_MAX_TOTAL];

  pthread_attr_t attr;
  pthread_t threads[MIMPI_MAX_TOTAL];
  pthread_mutex_t recv_mutex[MIMPI_MAX_TOTAL];

  bool waiting_for_recv[MIMPI_MAX_TOTAL];
  int waited_tag;
  int waited_count;
  pthread_cond_t waiting_place;
  pthread_cond_t waiting_place_feedback;
  int waiting_place_feedback_count;

  bool deadlock_they_wait[MIMPI_MAX_TOTAL];
  bool deadlock_tag[MIMPI_MAX_TOTAL];
  bool deadlock_count[MIMPI_MAX_TOTAL];
  message_info_node *deadlock_sent_queue[MIMPI_MAX_TOTAL];
} info;

bool equal_tags(int tag1, int tag2) {
  if (tag1 >= 0 && tag2 >= 0)
    return tag1 == tag2 || tag1 == MIMPI_ANY_TAG || tag2 == MIMPI_ANY_TAG;
  return tag1 == tag2;
}

bool ready_message(message *msg) {
  return msg->fraction_count == msg->total_fractions;
}

message *get_message(int source, int tag, int count) {
  message_node **current = &info.messages[source];
  message_node *temp;
  message *msg;

  while (*current != NULL) {
    msg = (*current)->msg;
    if (equal_tags(msg->tag, tag) && msg->size == count && ready_message(msg)) {
      temp = *current;
      *current = (*current)->next;
      free(temp);
      return msg;
    }

    current = &(*current)->next;
  }
  return NULL;
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
  message_node **current = &info.messages[source];
  while (*current != NULL) {
    if ((*current)->msg->id == frac.id) {
      add_fraction_to_message((*current)->msg, frac);
      return ready_message((*current)->msg);
    }
    current = &(*current)->next;
  }
  *current = create_node(create_message(frac));
  return ready_message((*current)->msg);
}

message_info_node *get_message_info_node(int tag, int count, int source) {
  message_info_node **current = &info.deadlock_sent_queue[source];
  while (*current != NULL) {
    if (equal_tags((*current)->tag, tag) && (*current)->count == count) {
      message_info_node *temp = *current;
      *current = (*current)->next;
      return temp;
    }
    current = &(*current)->next;
  }
  return NULL;
}

void handle_deadlock_recv(fraction *frac, int source) {
  message_info_node *msg = get_message_info_node(
      (int)frac->data[0], (int)frac->data[sizeof(int)], source);

  if (msg == NULL) {
    info.deadlock_they_wait[source] = true;
    info.deadlock_tag[source] = (int)frac->data[0];
    info.deadlock_count[source] = (int)frac->data[sizeof(int)];

    if (info.waiting_for_recv[source]) {
      info.waiting_for_recv[source] = false;
      int counter_value = info.waiting_place_feedback_count;
      ASSERT_ZERO(pthread_cond_signal(&info.waiting_place));
      while (info.waiting_place_feedback_count == counter_value) 
        ASSERT_ZERO(pthread_cond_wait(&info.waiting_place_feedback,
                                      &info.recv_mutex[source]));
    }
  } else {
    free(msg);
  }
}

void *worker(void *data) {
  int id = *((int *)data);
  fraction msg;

  assert(id != info.id);
  free(data);

  while (true) {
    memset(msg.data, 0, MSG_SIZE);

    //exit if we receive nothing
    if (chrecv(info.read_fd[id], &msg, sizeof(msg)) <= 0) {
      ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));
      info.has_finished[id] = true;
      if (info.waiting_for_recv[id]) {
        info.waiting_for_recv[id] = false;
        ASSERT_ZERO(pthread_cond_signal(&info.waiting_place));
      }
      ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
      return NULL;
    }

    ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[id]));

    if (msg.tag == MIMPI_SYNC_DEADLOCK_TAG) {
      handle_deadlock_recv(&msg, id);
    } else if (add_fraction(msg, id) && info.waiting_for_recv[id] &&
        equal_tags(msg.tag, info.waited_tag) &&
        info.waited_count == msg.total_size) {
      
      info.waiting_for_recv[id] = false;
      ASSERT_ZERO(pthread_cond_signal(&info.waiting_place));
    }

    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[id]));
  
  }

  return NULL;
}

void MIMPI_Init(bool enable_deadlock_detection) {
  channels_init();

  info.deadlock_detection = enable_deadlock_detection;
  info.next_mid = 0;
  info.id = atoi(getenv(MIMPI_ID));
  info.n = atoi(getenv(MIMPI_TOTAL));
  info.waiting_place_feedback_count = 0;

  pthread_cond_init(&info.waiting_place, NULL);
  pthread_cond_init(&info.waiting_place_feedback, NULL);
  
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
  char str1[MIMPI_ENOUGH_SPACE];
  char str2[MIMPI_ENOUGH_SPACE];
  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    MIMPI_READ_FD(i, info.id, str1);
    MIMPI_WRITE_FD(info.id, i, str2);
    info.read_fd[i] = atoi(getenv(str1));
    info.write_fd[i] = atoi(getenv(str2));
  }

  // init threads
  int detach_state = PTHREAD_CREATE_JOINABLE;

  // Create thread attributes.
  ASSERT_ZERO(pthread_attr_init(&info.attr));
  ASSERT_ZERO(pthread_attr_setdetachstate(&info.attr, detach_state));

  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    int *worker_arg = malloc(sizeof(int));
    *worker_arg = i;
    ASSERT_ZERO(pthread_create(&info.threads[i], &info.attr, worker, worker_arg));
  }
}

void free_messages(int id) {
  message_node *current = info.messages[id];
  message_node *temp;
  while (current != NULL) {
    temp = current;
    current = current->next;
    free(temp->msg->data);
    free(temp->msg);
    free(temp);
  }
}
void free_deadlock_queue(int id) {
  message_info_node *current = info.deadlock_sent_queue[id];
  message_info_node *temp;
  while (current != NULL) {
    temp = current;
    current = current->next;
    free(temp);
  }
}

void MIMPI_Finalize() {
  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    ASSERT_SYS_OK(close(info.read_fd[i]));
    ASSERT_SYS_OK(close(info.write_fd[i]));
  }

  for (int i = 0; i < info.n; i++) {
    if (i == info.id)
      continue;
    ASSERT_SYS_OK(pthread_join(info.threads[i], NULL));
  }
  ASSERT_ZERO(pthread_attr_destroy(&info.attr));

  // free queues 
  for(int i = 0; i < info.n; i++) {
    if(i == info.id)
      continue;
    free_messages(i);
    free_deadlock_queue(i);
  }
  channels_finalize();
}

int MIMPI_World_size() { return info.n; }

int MIMPI_World_rank() { return info.id; }

void add_info_node(int destination, int tag, int count) {
  if (info.deadlock_they_wait[destination] &&
      equal_tags(tag, info.deadlock_tag[destination]) &&
      count == info.deadlock_count[destination]) {

    info.deadlock_they_wait[destination] = false;
    info.deadlock_tag[destination] = 0;
    info.deadlock_count[destination] = 0;
    return;
  }

  message_info_node *node = malloc(sizeof(message_info_node));
  node->tag = tag;
  node->count = count;
  node->next = NULL;

  message_info_node **current = &info.deadlock_sent_queue[destination];
  while ((*current) != NULL) {
    current = &(*current)->next;
  }
  (*current) = node;
}

MIMPI_Retcode MIMPI_Send(void const *data, int count, int destination,
                         int tag) {
  // checks
  if (destination >= info.n || destination < 0) {
    return MIMPI_ERROR_NO_SUCH_RANK;
  }

  if (destination == info.id) {
    return MIMPI_ERROR_ATTEMPTED_SELF_OP;
  }

  if (info.deadlock_detection && tag > 0) {
    ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[destination]));
    add_info_node(destination, tag, count);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[destination]));
  }

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
    if (chsend(info.write_fd[destination], &msg, sizeof(msg)) < 0) {
      return MIMPI_ERROR_REMOTE_FINISHED;
    }
  }

  return MIMPI_SUCCESS;
}

MIMPI_Retcode MIMPI_Recv(void *data, int count, int source, int tag) {
  message *msg;

  if (source >= info.n || source < 0) {
    return MIMPI_ERROR_NO_SUCH_RANK;
  }

  if (source == info.id) {
    return MIMPI_ERROR_ATTEMPTED_SELF_OP;
  }

  ASSERT_ZERO(pthread_mutex_lock(&info.recv_mutex[source]));

  if (info.deadlock_detection && tag > 0) {
    int i[2] = {tag, count};
    MIMPI_Send(i, sizeof(i), source, MIMPI_SYNC_DEADLOCK_TAG);
  }

  if ((msg = get_message(source, tag, count)) != NULL) {
    memcpy(data, msg->data, count);
    free(msg->data);
    free(msg);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_SUCCESS;
  } else if (info.has_finished[source]) {
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_ERROR_REMOTE_FINISHED;
  } else if (info.deadlock_detection && info.deadlock_they_wait[source]) {
    info.deadlock_they_wait[source] = false;
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_ERROR_DEADLOCK_DETECTED;
  }

  info.waiting_for_recv[source] = true;
  info.waited_tag = tag;
  info.waited_count = count;

  while (info.waiting_for_recv[source])
    ASSERT_ZERO(pthread_cond_wait(&info.waiting_place, &info.recv_mutex[source]));

  info.waiting_place_feedback_count++;
  ASSERT_ZERO(pthread_cond_signal(&info.waiting_place_feedback));

  if ((msg = get_message(source, tag, count)) != NULL) {
    memcpy(data, msg->data, count);
    free(msg->data);
    free(msg);
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_SUCCESS;
  } else if (info.deadlock_detection && info.deadlock_they_wait[source]) {
    info.deadlock_they_wait[source] = false;
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_ERROR_DEADLOCK_DETECTED;
  } else {
    ASSERT_ZERO(pthread_mutex_unlock(&info.recv_mutex[source]));
    return MIMPI_ERROR_REMOTE_FINISHED;
  }
}

MIMPI_Retcode MIMPI_Barrier() {
  int d = 0;
  return MIMPI_Bcast(&d, sizeof(int), 0);
}

int treeid(int root, int id) { return (info.n + id - root) % info.n + 1; }
int realid(int root, int tree_id) { return (tree_id - 1 + root) % info.n; }
int son1(int id) { return 2 * id; }
int son2(int id) { return 2 * id + 1; }
int father(int id) { return id / 2; }

MIMPI_Retcode MIMPI_Bcast(void *data, int count, int root) {
  if(root >= info.n || root < 0) {
    return MIMPI_ERROR_NO_SUCH_RANK;
  }

  int tree_id = treeid(root, info.id);
  int s1 = son1(tree_id);
  int s2 = son2(tree_id);
  int d = 0;
  int i;

  if (s1 <= info.n) {
    if ((i = MIMPI_Recv(&d, 1, realid(root, s1), MIMPI_SYNC_TAG2)) !=
        MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (s2 <= info.n) {
    if ((i = MIMPI_Recv(&d, 1, realid(root, s2), MIMPI_SYNC_TAG2)) !=
        MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (tree_id != 1) {
    if ((i = MIMPI_Send(&d, 1, realid(root, father(tree_id)),
                        MIMPI_SYNC_TAG2)) != MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }

  if (tree_id != 1) {
    if ((i = MIMPI_Recv(data, count, realid(root, father(tree_id)),
                        MIMPI_SYNC_TAG1)) != MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (s1 <= info.n) {

    if ((i = MIMPI_Send(data, count, realid(root, s1), MIMPI_SYNC_TAG1)) !=
        MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }
  if (s2 <= info.n) {
    if ((i = MIMPI_Send(data, count, realid(root, s2), MIMPI_SYNC_TAG1)) !=
        MIMPI_SUCCESS)
      return MIMPI_ERROR_REMOTE_FINISHED;
  }

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
  if(root >= info.n || root < 0) {
    return MIMPI_ERROR_NO_SUCH_RANK;
  }

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
    ASSERT_SYS_OK(MIMPI_Send(my_data, count, realid(root, father(tree_id)),
                             MIMPI_SYNC_TAG1));
    ASSERT_SYS_OK(MIMPI_Recv(&d, sizeof(int), realid(root, father(tree_id)),
                             MIMPI_SYNC_TAG2));

  } else {
    memcpy(recv_data, my_data, count);
  }

  if (s1 <= info.n) {
    ASSERT_SYS_OK(
        MIMPI_Send(&d, sizeof(int), realid(root, s1), MIMPI_SYNC_TAG2));
  }
  if (s2 <= info.n) {
    ASSERT_SYS_OK(
        MIMPI_Send(&d, sizeof(int), realid(root, s2), MIMPI_SYNC_TAG2));
  }
  free(d1);
  free(d2);
  free(my_data);
  return MIMPI_SUCCESS;
}
