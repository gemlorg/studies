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

#define SIZE 0x3000

int main() {
  int n_contexts = ACCELDEV_MAX_CONTEXTS;

  int contexts_fds[ACCELDEV_MAX_CONTEXTS];

  for (int i = 0; i < n_contexts; i++) {
    contexts_fds[i] = do_open0();
  }

  do_open_with_error("/dev/acceldev0");

  for (int i = 0; i < n_contexts; i++) {
    do_close(contexts_fds[i]);
  }
}