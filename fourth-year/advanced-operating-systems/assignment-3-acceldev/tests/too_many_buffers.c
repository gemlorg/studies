#include <errno.h>
#include <fcntl.h>
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

#define SIZE 0x3000

int main() {
  int fd0 = do_open0();
  int fd1 = do_open0();
  struct acceldev_ioctl_create_buffer_result result;

  int n_buffers = ACCELDEV_NUM_BUFFERS;
  struct acceldev_ioctl_create_buffer_result results[ACCELDEV_NUM_BUFFERS];
  int buffers_descriptors[ACCELDEV_NUM_BUFFERS];
  bool used_slots[ACCELDEV_NUM_BUFFERS];

  memset(used_slots, 0, sizeof(used_slots));

  for (int i = 0; i < n_buffers; i++) {
    buffers_descriptors[i] =
        do_create_buf(fd0, SIZE, BUFFER_TYPE_DATA, &results[i]);
    int slot = results[i].buffer_slot;
    if (slot >= n_buffers || slot < 0) {
      fprintf(stderr, "Invalid buffer slot %d\n", slot);
      return -1;
    }
    if (used_slots[slot]) {
      fprintf(stderr, "Buffer slot already used %d\n", slot);
      return -1;
    }
    used_slots[slot] = true;
  }

  do_create_buf_with_error(fd0, SIZE, BUFFER_TYPE_DATA, &result);
  do_create_buf(fd1, SIZE, BUFFER_TYPE_DATA, &result);

  for (int i = 0; i < n_buffers; i++) {
    do_close(buffers_descriptors[i]);
  }

  do_close(fd0);
  do_close(fd1);
}
