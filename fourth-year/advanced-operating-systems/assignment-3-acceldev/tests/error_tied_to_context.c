#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

#include "common.h"

#define NUM_THREADS 40
#define SIZE 0x300000

struct test_buffer_run {
  int index;
};

void runner_thread(void *opaque) {
  struct test_buffer_run *data = opaque;

	int fd0 = do_open0();
  struct acceldev_ioctl_create_buffer_result code_result;

  int cfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_CODE, &code_result);

  uint32_t *code_buffer =
      (uint32_t *)mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, cfd, 0);

  if (code_buffer == MAP_FAILED)
    syserr("mmap");

  struct acceldev_ioctl_create_buffer_result result;

  int bfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_DATA, &result);

  uint32_t *data_buffer =
      (uint32_t *)mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, bfd, 0);

  if (data_buffer == MAP_FAILED)
    syserr("mmap");

  uint32_t n_cmds = 2;
  uint32_t commands_len = n_cmds * ACCELDEV_USER_CMD_WORDS;
  uint32_t offset = data->index * commands_len;
  uint32_t fill_value = data->index * 2 + 3;
  uint32_t fill_len = data->index * 15000 + 232;

  code_buffer[offset] = ACCELDEV_USER_CMD_TYPE_FILL;
  code_buffer[offset + 1] = fill_value;
  code_buffer[offset + 2] = result.buffer_slot;
  code_buffer[offset + 3] = data->index * sizeof(uint32_t);
  code_buffer[offset + 4] = fill_len;
  code_buffer[offset + 5] = ACCELDEV_USER_CMD_TYPE_FENCE;

	if (data->index % 5 == 1) {
		code_buffer[offset + 2] = ACCELDEV_NUM_BUFFERS + 3;
	}
	else if (data->index % 5 == 2) {
		code_buffer[offset + 4] = SIZE;
	}

  do_run(fd0, cfd, offset * sizeof(uint32_t),
         commands_len * sizeof(uint32_t));

	if (data->index % 5 == 1 || data->index % 5 == 2) {
		do_wait_for_err(fd0, 1);
		return;
	}
  do_wait(fd0, 1);

  for (int i = 0; i < data->index; i++)
    assert(data_buffer[i] == 0);

  for (int i = data->index; i < data->index + fill_len / sizeof(uint32_t); i++)
    assert(data_buffer[i] == fill_value);

  assert(data_buffer[data->index + fill_len / sizeof(uint32_t)] == 0);

  do_munmap(data_buffer, SIZE);
  do_close(bfd);
}

int main() {


  pthread_t threads[NUM_THREADS];
  struct test_buffer_run thread_data[NUM_THREADS];

  for (int i = 0; i < NUM_THREADS; i++) {
    thread_data[i].index = i;

    if (pthread_create(&threads[i], NULL, (void *(*)(void *))runner_thread,
                       &thread_data[i]) != 0) {
      perror("pthread_create");
      exit(EXIT_FAILURE);
    }
  }

  for (int i = 0; i < NUM_THREADS; i++) {
    if (pthread_join(threads[i], NULL) != 0) {
      perror("pthread_join");
      exit(EXIT_FAILURE);
    }
  }
}
