> Source: <https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/index.html>

# Assignment 3: Accelerator Device

Announcement date: 06.05.2025

Due date: 10.06.2025 (final due date 24.06.2025)

## Additional materials

- [Acceldev Device](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/device.html#z3-device)

- [Device simulator in QEMU](https://gitlab.uw.edu.pl/zso/2025l-public/zad3-public/-/tree/acceldev-public)

- For your driver, use the `acceldev.h` file from the simulator. Do not modify it, as tests will be run with the official version.

- [z3-tests-2025.tar.xz](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/../_downloads/8c9ebd35af40dadc482669f0766f765a/z3-tests-2025.tar.xz) public tests for the driver

## Introduction

CPUs are well suited for general-purpose programs, but various kinds of computations, e.g. computer graphics, scientific computing, and machine learning, can benefit significantly from using more specialized devices like GPUs and machine learning accelerators.

These devices are often realized as PCI cards. To expose their functionalities to user-space processes, higher-level abstractions such as CUDA, OpenCL, various graphics APIs (Vulkan, DirectX, OpenGL), and ONNX are typically used. To use those APIs with a specific device, device drivers are needed. These drivers translate higher-level operations into device instructions.

A device driver consists of both kernel mode and user mode code. Depending on specific software and hardware constraints, the *real* kernel mode driver can be either large or relatively small. With devices and APIs becoming increasingly complex, a recently occurring pattern is:

- the device includes a complex built-in chip with its own *device OS*

- complex user mode code handles APIs

- a relatively lightweight kernel mode driver connects the user mode code to the device and exposes OS functionality. Some operations traditionally implemented in the kernel are moved to user mode and the device itself.

In this task, you will implement a Linux PCI driver for a simplified imaginary device providing ONNX acceleration, called **Acceldev**.

The kernel driver should expose the device as a character device. For every **Acceldev** device attached, it should create a `/dev/acceldevX` character device, where `X` is the number of the attached device, starting from 0.

## Character device interface

The `/dev/acceldevX` device should support the following operations:

- `open`: allocates a new device context. This context will be used for sending commands. Support up to `ACCELDEV_MAX_CONTEXTS` open contexts, as each context should be registered in the device.

- `close`: closes the context, unregisters it from the device, and frees resources.

- `ioctl(ACCELDEV_IOCTL_CREATE_BUFFER)`: creates a code buffer (for submitting user commands) or a data buffer (for sharing memory with the device). Use `struct`` ``acceldev_ioctl_create_buffer`. A data buffer should be bound to a context slot; a code buffer should not be bound to any slot. `close` on the buffer should wait until all previously scheduled runs on the context are completed, then unbind the buffer from the device and free allocated DMA memory.

  The buffer should support `mmap` to allow reading and writing its contents in user mode and support `close`. No other operations (`read`, `write`, `ioctl`) are required.

  Validate arguments (return `EINVAL`), e.g. if the passed size is not less than or equal to `ACCELDEV_BUFFER_MAX_SIZE` (4 MiB).

- `ioctl(ACCELDEV_IOCTL_RUN)`: schedules the execution of user commands from a code buffer on a given context. See `struct`` ``acceldev_ioctl_run` and examples.

  Submit runs to the device using `ACCELDEV_DEVICE_CMD_TYPE_RUN`. Do not store extra run information in the driver unless absolutely necessary. If you need to wait for a specific run or device instruction, use `ACCELDEV_DEVICE_CMD_TYPE_FENCE` and interrupts.

  If there's insufficient space in `CMD_MANUAL_FEED`, queue the run in the driver until it can be submitted.

  Validate the arguments, including size and memory alignment. Return `EINVAL` on error. If the context previously encountered an error, return `EIO`.

- `ioctl(ACCELDEV_IOCTL_WAIT)`: waits for the completion of a specific `ACCELDEV_USER_CMD_TYPE_FENCE` submitted on a given context.

  `fence_wait` is the number of the fence command to wait for (across all submitted runs on this context) modulo `2^32`, starting from 1. The user mode driver is responsible for tracking the number of submitted fences.

  Return 0 on success, `EIO` on context error, and `EINTR` on interrupt.

Do not validate the user-submitted commands in code buffers. They are validated by the device. If an error occurs, the device sets the `error` status flag in `acceldev_context_on_device_config` for the given context and raises an interrupt. However, do validate arguments where it makes sense, e.g. ioctl calls.

The interface is more strictly defined by the provided examples and `acceldev.h`. When in doubt, ask.

## Solution format

The device driver should be implemented in C as a Linux kernel module, working with the lab's kernel version. The compiled module should be called `acceldev.ko`.

Submit an archive named `ab123456.tar.gz` (where `ab123456` is your students login). After unpacking, the package should create `ab123456` directory with the following contents:

- the module source files

- `Makefile` and `Kbuild` files — running `make` should build the `acceldev.ko` module

- a README file with a brief description of your solution, including driver design choices (e.g. regarding locking, fences) and code structure

## Grading

You can obtain up to 10 points. The assignment is graded based on automated tests and code review. The tests include the provided examples but also some other undisclosed tests which are variations of the provided examples

For the code review, points may be deducted for:

- detected errors, e.g. regarding locking or memory leaks

- minor deductions for issues like unclear or convoluted code structure

The driver may consist of a single source file if it's well-structured. However, modular and well-documented code is preferable.

## QEMU

**Acceldev** is implemented as a PCI device in QEMU.

To use the **Acceldev** device, a modified version of QEMU is required. It is available in source code form.

To compile it:

- Clone the repository: [https://gitlab.uw.edu.pl/zso/2025l-public/zad3-public.git](https://gitlab.uw.edu.pl/zso/2025l-public/zad3-public.git)

- Run: `git`` ``checkout`` ``acceldev-public`

- Ensure that the following dependencies are installed: **ncurses**, **libsdl**, **curl**, and in some distributions also **ncurses-dev**, **libsdl-dev**, **curl-dev** (package names may vary).

- Run `./configure` with the desired options. Suggested flags:

      --target-list=x86_64-softmmu --enable-virtfs --enable-gtk

- Change into the build directory:

      cd build

- Run `make` (or `ninja` if installed).

- Install with `make`` ``install` or run the binary directly (`build/qemu-system-x86_64`).

To emulate **Acceldev**:

- Pass the option `-device`` ``acceldev` to QEMU. Repeat it to emulate multiple devices.

To add the **Acceldev** device live (while QEMU is running):

- Enter QEMU monitor mode (Ctrl+Alt+2 inside the window)

- Type: `device_add`` ``acceldev`

- Return to the main screen (Ctrl+Alt+1)

- Run: `echo`` ``1`` ``>`` ``/sys/bus/pci/rescan` to detect the device in Linux

To simulate device removal:

- Run: `echo`` ``1`` ``>`` ``/sys/bus/pci/devices/0000:<device_id>/remove`

## Hints

To create buffer files, use `anon_inode_getfile` or `anon_inode_getfd`. To obtain a file struct from a file descriptor, use `fdget` and `fdput`. To check if the passed file structure is valid, verify its `file_operations`.

### `mmap` implementation

1.  Implement the `mmap` callback in `file_operations` to set `vm_ops` in the specified `vma` to your callbacks struct.

2.  In your `vm_operations_struct`, fill in the `fault` callback.

3.  In the `fault` callback:

    1.  Verify that `pgoff` is within buffer size or return `VM_FAULT_SIGBUS`.

    2.  Get the virtual address (in kernel space) of the appropriate buffer page and translate it with `virt_to_page` to `struct`` ``page`` ``*`.

    3.  Increase the page refcount with `get_page`.

    4.  Set the `page` field in `vm_fault`.

    5.  Return 0.

## Extras – ONNX Runtime

For real applications, the kernel mode driver would be part of a larger package with a user mode driver.

For ML accelerators, a popular choice is [ONNX](https://onnx.ai/onnx/intro/index.html), which provides tools for converting machine learning models (e.g. created using scikit-learn, PyTorch, or TensorFlow) into the ONNX format. This format saves models as graphs of [ONNX operators](https://onnx.ai/onnx/operators/index.html), including both simple (e.g. *Abs*, *Vector Addition*) and complex operations like *Transformer Attention*.

To accelerate such a model, the accelerator must support some ONNX operators. This integration can be accomplished using [ONNX Runtime](https://onnxruntime.ai/docs/execution-providers/) by registering a new Execution Provider for the device. This provider informs ONNX Runtime which operations are supported and converts them to device instructions using APIs such as NVIDIA CUDA, AMD ROCm, or the kernel driver.


---

# Acceldev Device

The **Acceldev** device is attached to the computer via the PCI bus. You will find the necessary information in `acceldev.h`.

The device does not have memory of its own and uses the main memory of the computer with Direct Memory Access (DMA). To overcome memory fragmentation, it uses virtual addresses and page tables in its specific format.

The device supports `ACCELDEV_MAX_CONTEXTS` (255) independent contexts, which do not share memory. These contexts should be tied to the driver contexts.

The device is designed to allow user commands to run without additional kernel mode validation, while ensuring that users cannot access system memory or other contexts on the device. If user commands are invalid or trigger an error, the device marks the context as errored and raises an interrupt to notify the driver.

Ensuring fair compute time across contexts is not guaranteed; one context may occupy the device, preventing others from running. Dealing with this issue is outside the scope of this assignment.

The device is controlled using MMIO registers. It has only one BAR (BAR0), used for these registers, and uses a single PCI interrupt line.

The MMIO area is 64 KiB in size, but only some of this range is used for registers. All documented registers are 32-bit, little-endian format, and should be accessed only through aligned 32-bit reads and writes.

## Buffers and Paging

Data and commands used by the device are stored in paged buffers. Each context can have `ACCELDEV_NUM_BUFFERS` (16) buffers bound. These are configured using `ACCELDEV_CONTEXTS_CONFIGS` with an array of `acceldev_context_on_device_config` structures and the `ACCELDEV_DEVICE_CMD_TYPE_BIND_SLOT` device command.

Except for `ACCELDEV_CONTEXTS_CONFIGS`, which uses 64-bit contiguous memory, all buffers and page tables use 40-bit physical addresses, 22-bit virtual addresses in the buffer, and pages of size `ACCELDEV_PAGE_SIZE` (4 KiB). The page tables are single-level.

- The kernel passes a 64-bit physical address of the buffer's page table to the device.

- Bits 12–21 of the virtual address select the page table entry, which contains the physical address of the page.

- Bits 0–11 of the virtual address represent the offset within the page.

Page tables are 4 KiB in size and contain 1024 entries, each being a 32-bit little-endian word. Each page table entry has the following format:

- Bit 0: `PRESENT` — if set, the entry is valid. If not set, using the entry raises a `MEM_ERROR`.

- Bits 4–31: `PA` — bits 12–39 of the page's physical address. Bits 0–11 are always zero; pages must be aligned.

## Sending Commands

The device supports two types of commands:

- Device commands, sent and validated by the driver.

- User commands (also called context commands), sent by running a code buffer via the `ACCELDEV_DEVICE_CMD_TYPE_RUN` command.

### Device Commands

Device commands consist of `ACCELDEV_DEVICE_CMD_WORDS` (5) 32-bit little-endian words. They are sent via the `CMD_MANUAL_FEED` registers:

- `BAR0`` ``+`` ``0x008c:`` ``CMD_MANUAL_FREE` Read-only register. Shows how many full commands may be queued before a `FEED_ERROR` occurs. The queue holds `CMDS_BUFFER_SIZE` (255) commands. Assume the queue is empty after a device reset.

- `BAR0`` ``+`` ``0x008c`` ``–`` ``BAR0`` ``+`` ``0x009c:`` ``CMD_MANUAL_FEED` Five write-only registers for writing command words. Writing the last (4th counting from 0) word at `BAR0`` ``+`` ``0x009c` submits the command. Submitting when the queue is full (`CMD_MANUAL_FREE`` ``==`` ``0`) raises a `FEED_ERROR` interrupt.

#### NOP Command

Does nothing. Can be used to fill the queue if you feel like it.

- 0th word: header - Command type: `0x0`

The other words are unused. To submit the command you only need to write the 0th and 4th words.

#### FENCE Command

Signals that all commands submitted before it have been processed.

- 0th word: header - Command type: `0x3`

- 1st word: 32-bit value `VAL`

Behavior:

1.  Waits for completion of all previous commands.

2.  Sets `CMD_FENCE_LAST` to `VAL`.

3.  If `VAL`` ``==`` ``CMD_FENCE_WAIT`, triggers `FENCE_WAIT` interrupt.

Registers:

- `BAR0`` ``+`` ``0x00a0:`` ``CMD_FENCE_LAST` 32-bit read/write register. Set to `VAL` while processing FENCE.

- `BAR0`` ``+`` ``0x00a4:`` ``CMD_FENCE_WAIT` 32-bit read/write register. Used to schedule `FENCE_WAIT` interrupt.

#### RUN command

Schedules user commands on a context.

- 0th word: header - Bits 0–3: command type `0x1` - Bits 4–31: context ID

- 1st–2nd words: lower/upper 32 bits of code buffer page table address

- 3rd word: offset (in bytes) of the first command

- 4th word: size (in bytes) of commands to process (`n_commands`` ``*`` ``ACCELDEV_USER_CMD_WORDS`` ``*`` ``sizeof(uint32_t)`)

#### BIND_SLOT Command

Binds or unbinds a data buffer to a slot for a given context. Binding and unbiding buffers can also be realized using `ACCELDEV_CONTEXTS_CONFIGS` but that method is unsafe to execute while the device may use the bound buffers. Therefore, `BIND_SLOT` is preferred when the context is already running.

- 0th word: header - Bits 0–3: command type `0x2` - Bits 4–31: context ID

- 1st word: slot number

- 2nd–3rd words: lower/upper 32 bits of data buffer page table address

Unbiding a buffer is done by replacing it with a new buffer or submitting 0 as the buffer page table address.

### User Commands

User commands are `ACCELDEV_USER_CMD_WORDS` 32-bit aligned little-endian words in a DMA buffer. Always write the full number of words, even if the command is shorter.

Supported commands:

- `NOP`` ``(0x0)`

- `FENCE`` ``(0x1)`

- `FILL`` ``(0x2)`

#### FENCE Command

1.  Waits for previous user commands to finish.

2.  Increments `fence_counter` in the context config.

3.  Triggers `USER_FENCE_WAIT` interrupt.

- 0th word: header - Command type: `0x1`

#### FILL Command

Fills part of a buffer with a value.

- 0th word: header - Command type: `0x2`

- 1st word: 32-bit value

- 2nd word: buffer slot

- 3rd word: start offset (bytes)

- 4th word: length (bytes)

A real accelerator would support more interesting commands but as the goal here is system programming, those will suffice. If you are interested in this topic, refer to [Extras – ONNX Runtime](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/index.html#z3-driver-onnx).

## Control Registers

`BAR0`` ``+`` ``0x0008:`` ``ENABLE` Controls whether the device processes commands. If 0, then the commands are not processed.

`BAR0`` ``+`` ``0x000c`` ``–`` ``0x0010:`` ``CONTEXTS_CONFIGS` Attaches the contexts' configuration memory. The configuration is stored in contiguous DMA memory containing an array of `acceldev_context_on_device_config` configs for each of the `ACCELDEV_CONTEXTS_CONFIGS` contexts.

- `BAR0`` ``+`` ``0x000c`: lower 32 bits of address

- `BAR0`` ``+`` ``0x0010`: upper 32 bits of address

## Interrupts

The device uses six internal interrupts, all multiplexed into one PCI interrupt.

- **FENCE_WAIT** — completion of a FENCE command

- **FEED_ERROR** — command queue full

- **CMD_ERROR** — invalid device command

- **MEM_ERROR** — invalid memory access

- **SLOT_ERROR** — request for inactive slot

- **USER_FENCE_WAIT** — user FENCE command triggered

An interrupt becomes *active* on event occurrence and *inactive* when cleared by writing 1 to its bit in the `INTR` register.

Independently, each of the above interrupts can also be either enabled or disabled at any given time. The driver can set an enabled subset of interrupts by writing an appropriate mask to the INTR_ENABLE register. The device will signal an interrupt on its PCI interrupt line if and only if there is an interrupt that is both enabled and active.

`BAR0`` ``+`` ``0x0000:`` ``INTR` Interrupt status register. Each bit corresponds to an interrupt. Reading returns 1 for active, 0 for inactive. Writing resets (sets to inactive) all interrupts for which 1s were written.

`BAR0`` ``+`` ``0x0004:`` ``INTR_ENABLE` Interrupt enable register. Same bit layout as `INTR`. A bit value of 1 enables the interrupt. On reset, this is set to 0. Upon machine reset, the register is set to 0, blocking the device from signaling a PCI interrupt until the driver is loaded.

## Starting the Device

To start the device, follow these steps:

1.  Clear `INTR` by writing all 1s.

2.  Enable required interrupts via `INTR_ENABLE`.

3.  Attach context configs in `ACCELDEV_CONTEXTS_CONFIGS`.

4.  Enable all device blocks via `ENABLE`.

5.  Optionally set `CMD_FENCE_LAST` and `CMD_FENCE_WAIT`.

To shut down, write 0 to both `ENABLE` and `INTR_ENABLE`.

If the device reports an error, reset it by repeating the start-up procedure.
