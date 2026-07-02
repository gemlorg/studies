#include <signal.h>
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
	struct acceldev_ioctl_create_buffer_result result;

	int bfd = do_create_buf(fd, SIZE, BUFFER_TYPE_CODE, &result);
	do_run(fd, bfd, 0, ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t)); //just NOPs because the buffer is zeroed
	do_wait(fd, 0);
	do_run_with_err(fd, 1, 0, ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t)); //run with wrong buffer descriptor
	do_close(bfd);
	do_close(fd);
	return 0;
}