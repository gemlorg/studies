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


static void chk(int fd, int req) {
	if (ioctl(fd, req, 0x01) != -1) {
		fprintf(stderr, "ioctl %d succeeded\n", req);
		exit(-1);
	}
}

int main() {
	int fd = do_open0();
	chk(fd, 0x50);
	
	do_close(fd);
	return 0;
}