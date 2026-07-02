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

#define SIZE 0x3000

//tests that code buffer does not occupy slot

int main()
{
		int fd0 = do_open0();
		int fd1 = do_open0();

		struct acceldev_ioctl_create_buffer_result result;

		int cfd = do_create_buf(fd0, SIZE, BUFFER_TYPE_CODE, &result);

		int n_buffers = ACCELDEV_NUM_BUFFERS;
		struct acceldev_ioctl_create_buffer_result results[ACCELDEV_NUM_BUFFERS];
		int buffers_descriptors[ACCELDEV_NUM_BUFFERS];
		bool used_slots[ACCELDEV_NUM_BUFFERS];

		memset(used_slots, 0, sizeof(used_slots)); 

		for (int i=0; i < n_buffers; i++)
		{
			buffers_descriptors[i] = do_create_buf(fd0, SIZE, BUFFER_TYPE_DATA, &results[i]);
			int slot = results[i].buffer_slot;
			if (slot >= n_buffers || slot < 0) {
				fprintf(stderr, "Invalid buffer slot %d\n", slot);
				return -1;
			}
			if (used_slots[slot]) {
				fprintf(stderr, "Buffer slot already used %d\n", slot);
				return -1;
			}
			used_slots[slot] = true;
		}

		for (int i=0; i < n_buffers; i++)
		{
			do_close(buffers_descriptors[i]);
		}

		do_close(fd0);
		do_close(fd1);
}
