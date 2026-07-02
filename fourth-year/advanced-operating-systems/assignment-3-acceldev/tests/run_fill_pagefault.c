#include <assert.h>
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

  struct acceldev_ioctl_create_buffer_result result;

  int cfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_CODE, &result);

  uint32_t *code_buffer =
      (uint32_t *)mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, cfd, 0);

  if (code_buffer == MAP_FAILED)
    syserr("mmap");

  int bfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_DATA, &result);

  uint32_t *data_buffer =
      (uint32_t *)mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, bfd, 0);

  if (data_buffer == MAP_FAILED)
    syserr("mmap");

  uint32_t start = 3;
  uint32_t len = 13000;
  uint32_t value = 11;

  code_buffer[100] = ACCELDEV_USER_CMD_TYPE_FILL;
  code_buffer[101] = value;
  code_buffer[102] = result.buffer_slot;
  code_buffer[103] = start * sizeof(uint32_t);
  code_buffer[104] = len;
  code_buffer[105] = ACCELDEV_USER_CMD_TYPE_FENCE;

  uint32_t n_cmds = 2;

  do_run(fd0, cfd, 100 * sizeof(uint32_t),
         n_cmds * ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t));
  do_wait_for_err(fd0, 1);

  do_munmap(code_buffer, SIZE);
  do_munmap(data_buffer, SIZE);
  do_close(cfd);
  do_close(bfd);
}
