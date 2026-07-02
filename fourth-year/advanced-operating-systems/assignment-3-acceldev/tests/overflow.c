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

void signal_handler(int i) {
	exit(0);
}

int main() {
	int fd = do_open0();
  struct acceldev_ioctl_create_buffer_result result;
	int bfd = do_create_buf(fd, SIZE, 0x0, &result);
	char *buffer = (char *) mmap(0, SIZE + 0x1000, PROT_READ | PROT_WRITE, MAP_SHARED, bfd, 0);

	if (signal(SIGBUS, signal_handler) == SIG_ERR) {
		fprintf(stderr, "Cannot set signal handler\n");
		return -1;
	}

	if (buffer == MAP_FAILED)
		syserr("mmap");

	for (int i = 0; i < 0x1000; i++)
		buffer[i + SIZE] = 0x13;

	fprintf(stderr, "Should not reach this point\n");
	return -1;
}
