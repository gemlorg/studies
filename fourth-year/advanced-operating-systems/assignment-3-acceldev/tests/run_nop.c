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

  int bfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_CODE, &result);

  uint32_t *data_buffer =
      (uint32_t *)mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, bfd, 0);

  if (data_buffer == MAP_FAILED)
    syserr("mmap");

  for (int i = 0; i < SIZE / sizeof(uint32_t); i++)
    code_buffer[i] = 0x13;

  for (int i = 10; i < 10 + ACCELDEV_USER_CMD_WORDS * 5; i++)
    code_buffer[i] = ACCELDEV_USER_CMD_TYPE_NOP;

  code_buffer[10 + ACCELDEV_USER_CMD_WORDS * 5] = ACCELDEV_USER_CMD_TYPE_FENCE;

  do_run(fd0, cfd, 10 * sizeof(uint32_t),
         ACCELDEV_USER_CMD_WORDS * 6 * sizeof(uint32_t));

  // do_wait(fd0, 1);

  do_munmap(code_buffer, SIZE);
  do_munmap(data_buffer, SIZE);
  do_close(bfd);
  do_close(cfd);
}
