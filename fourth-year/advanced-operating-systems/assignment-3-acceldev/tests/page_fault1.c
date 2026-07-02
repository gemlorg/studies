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

#include "../acceldev.h"

#include "common.h"

// can be handled either by checking the ioctl args or getting MEM_ERROR from
// the device when it tries to access unbound page

int main() {
  int fd = do_open0();
  struct acceldev_ioctl_create_buffer_result result;

  int bfd = do_create_buf(fd, 0x1000, BUFFER_TYPE_CODE, &result);

  do_run_and_wait_with_err(fd, bfd, 0x1000, 1);
  do_close(fd);
  return 0;
}