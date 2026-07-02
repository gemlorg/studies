#include "acceldev_driver.h"

#define LOWER_32(x) ((uint32_t)(x))
#define UPPER_32(x) ((uint32_t)((x) >> 32))

void acceldev_iow(struct acceldev_dev *dev, uint32_t reg, uint32_t val) {
  iowrite32(val, dev->bar + reg);
}
uint32_t acceldev_ior(struct acceldev_dev *dev, uint32_t reg) {
  uint32_t res = ioread32(dev->bar + reg);
  return res;
}

void acceldev_feed_cmd(struct acceldev_dev *dev, uint32_t *cmd_words) {
  int i;
  while (acceldev_ior(dev, CMD_MANUAL_FREE) < ACCELDEV_DEVICE_CMD_WORDS)
    ;
  for (i = 0; i < ACCELDEV_DEVICE_CMD_WORDS; i++) {
    acceldev_iow(dev, CMD_MANUAL_FEED + i * 4, cmd_words[i]);
  }
}

void acceldev_submit_bind_slot(struct acceldev_buffer *buf) {
  uint32_t cmd[ACCELDEV_DEVICE_CMD_WORDS];
  dma_addr_t pt_dma = buf->pt->pt.dev;
  struct acceldev_dev *adev = buf->ctx->adev;
  cmd[0] = ACCELDEV_DEVICE_CMD_BIND_SLOT_HEADER(buf->ctx->id);
  cmd[1] = buf->id;
  cmd[2] = LOWER_32(pt_dma);
  cmd[3] = UPPER_32(pt_dma);
  cmd[4] = 0;
  acceldev_feed_cmd(adev, cmd);
}

void acceldev_submit_unbind_slot(struct acceldev_buffer *buf) {
  uint32_t cmd[ACCELDEV_DEVICE_CMD_WORDS];
  struct acceldev_dev *adev = buf->ctx->adev;
  cmd[0] = ACCELDEV_DEVICE_CMD_BIND_SLOT_HEADER(buf->ctx->id);
  cmd[1] = buf->id;
  cmd[2] = 0;
  cmd[3] = 0;
  cmd[4] = 0;
  acceldev_feed_cmd(adev, cmd);
}

void acceldev_submit_run(struct acceldev_dev *adev,
                         struct acceldev_task *task) {
  uint32_t cmd[ACCELDEV_DEVICE_CMD_WORDS];
  dma_addr_t pt_dma = task->buf->pt->pt.dev;
  cmd[0] = ACCELDEV_DEVICE_CMD_RUN_HEADER(task->buf->ctx->id);
  cmd[1] = LOWER_32(pt_dma);
  cmd[2] = UPPER_32(pt_dma);
  cmd[3] = task->offset;
  cmd[4] = task->size;
  acceldev_feed_cmd(adev, cmd);
}

void start_device(struct acceldev_dev *adev) {
  /* 1) Clear all pending interrupts */
  acceldev_iow(adev, ACCELDEV_INTR, 0xFFFFFFFFU);
  /* 2) Enable required interrupts */
  acceldev_iow(adev, ACCELDEV_INTR_ENABLE,
               ACCELDEV_INTR_FENCE_WAIT | ACCELDEV_INTR_FEED_ERROR |
                   ACCELDEV_INTR_CMD_ERROR | ACCELDEV_INTR_MEM_ERROR |
                   ACCELDEV_INTR_SLOT_ERROR | ACCELDEV_INTR_USER_FENCE_WAIT);
  /* 3) Allocate and attach context config table */
  uint32_t ctx_lo = (uint32_t)adev->mgr->contexts_dma;
  uint32_t ctx_hi = (uint32_t)(adev->mgr->contexts_dma >> 32);
  acceldev_iow(adev, ACCELDEV_CONTEXTS_CONFIGS, ctx_lo);
  acceldev_iow(adev, ACCELDEV_CONTEXTS_CONFIGS + 4, ctx_hi);
  /* 4) Enable all device blocks */
  acceldev_iow(adev, ACCELDEV_ENABLE, 1);
}

void acceldev_restart_device(struct acceldev_dev *adev) {
  acceldev_iow(adev, ACCELDEV_ENABLE, 0);
  acceldev_iow(adev, ACCELDEV_INTR_ENABLE, 0);
  acceldev_ctxmgr_fail_contexts(adev);

  start_device(adev);
}
