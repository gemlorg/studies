#include "../acceldev.h"
#include <stdbool.h>
#include <stdint.h>

void syserr(const char *fmt) {
  fprintf(stderr, "ERROR %s (%d; %s)\n", fmt, errno, strerror(errno));
  exit(1);
}

int do_open(char *path) {
  int fd;
  if ((fd = open(path, O_RDWR)) < 0)
    syserr("open");
  return fd;
}

int do_open_with_error(char *path) {
	if ((open(path, O_RDWR)) == 0)
		syserr("open should fail");
}

int do_open0() { return do_open("/dev/acceldev0"); }

int do_open1() { return do_open("/dev/acceldev1"); }

int do_create_buf(int fd, int size, enum acceldev_buffer_type type,
                  struct acceldev_ioctl_create_buffer_result *result) {
  struct acceldev_ioctl_create_buffer buf = {size, type, result};

  int bfd;
  if ((bfd = ioctl(fd, ACCELDEV_IOCTL_CREATE_BUFFER, &buf)) < 0)
    syserr("create_buffer");
  return bfd;
}

void do_create_buf_with_error(
    int fd, int size, enum acceldev_buffer_type type,
    struct acceldev_ioctl_create_buffer_result *result) {
  struct acceldev_ioctl_create_buffer buf = {size, type, result};

  int bfd;
  if ((bfd = ioctl(fd, ACCELDEV_IOCTL_CREATE_BUFFER, &buf)) != -1)
    syserr("create_buf should fail");
}

void do_close(int fd) {
  if (close(fd) < 0)
    syserr("close");
}

void do_munmap(void *addr, size_t len) {
  if (munmap(addr, len) < 0)
    syserr("munmap");
}

void do_run(int fd, int cfd, uint32_t addr, uint32_t size) {
  struct acceldev_ioctl_run run = {cfd, addr, size};
  int i;
  if (ioctl(fd, ACCELDEV_IOCTL_RUN, &run))
    syserr("run");
}

void do_run_with_err(int fd, int cfd, uint32_t addr, uint32_t size) {
  struct acceldev_ioctl_run run = {cfd, addr, size};
  int i;
  if (ioctl(fd, ACCELDEV_IOCTL_RUN, &run) != -1)
    syserr("run should fail");
}

void do_wait(int fd, uint32_t fence_wait) {
  struct acceldev_ioctl_wait wait = {fence_wait};
  if (ioctl(fd, ACCELDEV_IOCTL_WAIT, &wait))
    syserr("wait");
}

void do_wait_for_err(int fd, uint32_t fence_wait) {
  struct acceldev_ioctl_wait wait = {fence_wait};
  if (ioctl(fd, ACCELDEV_IOCTL_WAIT, &wait) != -1)
    syserr("wait should fail");
}

void do_run_and_wait_with_err(int fd, int cfd, uint32_t addr, uint32_t size) {
  struct acceldev_ioctl_run run = {cfd, addr, size};
  ioctl(fd, ACCELDEV_IOCTL_RUN, &run); // ignore
  do_wait_for_err(fd, 0);
}
