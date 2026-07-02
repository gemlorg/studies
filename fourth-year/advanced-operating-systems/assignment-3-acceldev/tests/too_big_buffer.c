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
  struct acceldev_ioctl_create_buffer_result result1, result2;

	int bfd1 = do_create_buf(fd0, SIZE, BUFFER_TYPE_DATA, &result1);

	do_create_buf_with_error(fd0, ACCELDEV_BUFFER_MAX_SIZE + 3, BUFFER_TYPE_DATA, &result2);

	do_close(bfd1);
  do_close(fd0);
}
