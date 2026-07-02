#include <fcntl.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "../acceldev.h"

#include "common.h"
#define SIZE 0x3000

int main() {
	int fd = do_open0();
	int fd2 = do_open0();
  struct acceldev_ioctl_create_buffer_result result;
	int bfd = do_create_buf(fd, SIZE, BUFFER_TYPE_CODE, &result);
	do_run(fd, bfd, 0, 4);
	do_run_with_err(fd2, bfd, 0, 4);

	do_close(bfd);
	do_close(fd);
	do_close(fd2);

	return 0;
}