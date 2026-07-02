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

#include "common.h"

#define SIZE 0x300000

//tests that wait wakes on error

int main()
{
		int fd0 = do_open0();

		struct acceldev_ioctl_create_buffer_result result;

		int cfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_CODE, &result);
		
		uint32_t *code_buffer = (uint32_t*) mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, cfd, 0);

		if (code_buffer == MAP_FAILED)
			syserr("mmap");

		for (int i=0x50123; i < 0x56000; i++)
			code_buffer[i] = i;

		do_run(fd0, cfd, 0, SIZE);

		do_wait_for_err(fd0, 100000);
		do_munmap(code_buffer, SIZE);
		do_close(cfd);
}
