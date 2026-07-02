#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

#include "../acceldev.h"

#include "common.h"
#define SIZE 0x3000

int main() {
  int fd = do_open0();
  struct acceldev_ioctl_create_buffer_result result;
  int bfd = do_create_buf(fd, SIZE, BUFFER_TYPE_DATA, &result);
  do_run_with_err(
      fd, bfd, 0,
      7 * ACCELDEV_USER_CMD_WORDS *
          sizeof(uint32_t)); // just NOPs because the buffer is zeroed
  do_close(bfd);
  do_close(fd);
  return 0;
}