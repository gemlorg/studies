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

#define SIZE 0x1000

// tests that invalid user command results in breaking a context
// running a broken context fails immediately

int main() {
	int fd = do_open0();
	struct acceldev_ioctl_create_buffer_result result;
	int cfd = do_create_buf(fd, SIZE, BUFFER_TYPE_CODE, &result);

	char *buffer = (char *) mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, cfd, 0);
	if (buffer == MAP_FAILED)
		syserr("mmap");

	for (int i = 0; i < 0x10; i++)
		buffer[i] = i % 2 ? 0xab : 0xcd;

	do_run_and_wait_with_err(fd, cfd, 0, ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t) * 10);

	for (int i = 0; i < 0x10; i++)
		buffer[i] = 0; //all nops
	
	do_run_with_err(fd, cfd, 0, ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t) * 10);

	do_close(cfd);
	do_close(fd);
	return 0;
}
