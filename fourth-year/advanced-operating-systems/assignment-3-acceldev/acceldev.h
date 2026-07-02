#ifndef ACCELDEV_H
#define ACCELDEV_H
#include <linux/ioctl.h>
#include <linux/types.h>
typedef uint64_t dma_addr_t;

#define ACCELDEV_NAME "acceldev" // defined for some safety, to avoid typos

/* Section 1: PCI ids. */
#define ACCELDEV_VENDOR_ID 0x0666
#define ACCELDEV_DEVICE_ID 0x019

/* Section 2: MMIO registers.  */

/* Interrupt status.  */
#define ACCELDEV_INTR 0x0000
#define ACCELDEV_INTR_ENABLE 0x0004
#define ACCELDEV_ENABLE 0x0008
#define ACCELDEV_CONTEXTS_CONFIGS 0x000c // this one is 2x32bit

#define CMDS_BUFFER_SIZE 255
#define CMD_MANUAL_FEED                                                        \
  0x008c // 5x32bit - writing the last one submits the command
#define CMD_MANUAL_FREE 0x008c

#define ACCELDEV_BAR_SIZE 0x1000

#define ACCELDEV_CMD_FENCE_LAST 0x00a0
#define ACCELDEV_CMD_FENCE_WAIT 0x00a4

/* Section 3: misc constants, enums etc.  */
#define ACCELDEV_DEVICE_CMD_WORDS 5 // how many 32bit words per command
#define ACCELDEV_USER_CMD_WORDS 5
#define ACCELDEV_PAGE_SHIFT 12
#define ACCELDEV_PAGE_SIZE 0x1000
#define ACCELDEV_NUM_BUFFERS 16
#define ACCELDEV_BUFFER_MAX_SIZE 0x400000
#define ACCELDEV_MAX_CONTEXTS 255

/* Interrupt flags */
#define ACCELDEV_INTR_FENCE_WAIT 0x1
#define ACCELDEV_INTR_FEED_ERROR 0x2
#define ACCELDEV_INTR_CMD_ERROR 0x4
#define ACCELDEV_INTR_MEM_ERROR 0x8
#define ACCELDEV_INTR_SLOT_ERROR 0x10
#define ACCELDEV_INTR_USER_FENCE_WAIT 0x20

/* ioctls */
#define ACCELDEV_IOCTL_CREATE_BUFFER 0x0
#define ACCELDEV_IOCTL_RUN 0x1
#define ACCELDEV_IOCTL_WAIT 0x3

/* Device Commands */
#define ACCELDEV_DEVICE_CMD_TYPE(CMD) (CMD & 0xf)
#define ACCELDEV_DEVICE_CMD_TYPE_NOP 0x0
#define ACCELDEV_DEVICE_CMD_TYPE_RUN 0x1
#define ACCELDEV_DEVICE_CMD_TYPE_BIND_SLOT 0x2
#define ACCELDEV_DEVICE_CMD_TYPE_FENCE 0x3

#define ACCELDEV_DEVICE_CMD_RUN_HEADER(CONTEXT_ID)                             \
  (ACCELDEV_DEVICE_CMD_TYPE_RUN | CONTEXT_ID << 4)
#define ACCELDEV_DEVICE_CMD_RUN_EXTRACT_CONTEXT_ID(HEADER) (HEADER >> 4)

#define ACCELDEV_DEVICE_CMD_BIND_SLOT_HEADER(CONTEXT_ID)                       \
  (ACCELDEV_DEVICE_CMD_TYPE_BIND_SLOT | CONTEXT_ID << 4)
#define ACCELDEV_DEVICE_CMD_BIND_SLOT_EXTRACT_CONTEXT_ID(HEADER) (HEADER >> 4)

/* User Commands */
#define ACCELDEV_USER_CMD_TYPE(CMD) (CMD & 0xf)
#define ACCELDEV_USER_CMD_TYPE_NOP 0x0
#define ACCELDEV_USER_CMD_TYPE_FENCE 0x1
#define ACCELDEV_USER_CMD_TYPE_FILL 0x2

#define ACCELDEV_CONTEXT_STATUS_ERROR 0x1

enum acceldev_buffer_type {
  BUFFER_TYPE_CODE = 0,
  BUFFER_TYPE_DATA = 1,
  BUFFER_TYPES_COUNT,
};

struct acceldev_ioctl_create_buffer_result {
  uint32_t buffer_slot;
};

struct acceldev_ioctl_create_buffer {
  // args
  int size;
  enum acceldev_buffer_type type;
  // results
  struct acceldev_ioctl_create_buffer_result *result;
};

struct acceldev_ioctl_run {
  int cfd;       // code buffer file descriptor
  uint32_t addr; // bytes since buffer start
  uint32_t size; // in bytes
};

struct acceldev_ioctl_wait {
  uint32_t fence_wait; // number of a fence command on this context (across all
                       // submitted runs) MOD 2^32, starting from 1
};

// the fields are generally read-only for the driver, filled by the device
// with the exclusion that on init or `resume` before passing the address to
// `ACCELDEV_CONTEXTS_CONFIGS` you may have pre-configured memory
// remember to zero when freeing a context
struct acceldev_context_on_device_config {
  dma_addr_t
      buffers_slots_config_ptr[ACCELDEV_NUM_BUFFERS]; // for each buffer:
                                                      // 64bit dma_addr_t
                                                      // pointing to page table
  uint32_t fence_counter;
  uint8_t status;
} __attribute__((aligned(64))); // Align to cacheline (64 bytes)

static inline bool acceldev_context_on_device_config_is_error(uint8_t status) {
  return status & ACCELDEV_CONTEXT_STATUS_ERROR;
}


#endif