# acceldev

A Linux kernel driver for **acceldev**, an imaginary ML-accelerator PCI card emulated by a patched QEMU. 

## Interface

Every attached card appears as `/dev/acceldevX`. Opening the device allocates an independent *context* . A context supports three ioctls:

- **`ACCELDEV_IOCTL_CREATE_BUFFER`** — creates a *code* or *data* buffer and returns a file descriptor for it. The buffer fd supports `mmap` for reading and writing its contents from user space. Data buffers are bound to one of 16 slots on the context so device commands can address them; code buffers hold the user commands themselves.
- **`ACCELDEV_IOCTL_RUN`** — schedules execution of a range of user commands from a code buffer on the context.
- **`ACCELDEV_IOCTL_WAIT`** — blocks until the n-th user `FENCE` command submitted on the context has completed (0 on success, `EIO` if the context errored, `EINTR` on signal).


## Building

Inside the ccourse-supplied kernel and QEMU:

```bash
make && make install && reboot
```


## Tests

`tests/` contains the public test suite; build it with the Makefile there and run `./run.sh`, which executes the tests.
