
#include "acceldev_driver.h"

void acceldev_fail_ctx(struct acceldev_ctx *ctx) {
  unsigned long flags;
  spin_lock_irqsave(&ctx->adev->dev_lock, flags);
  ctx->failed = true;
  wake_up_interruptible_all(&ctx->adev->fence_queue);
  spin_unlock_irqrestore(&ctx->adev->dev_lock, flags);
}
bool acceldev_ctx_failed(struct acceldev_ctx *ctx) {
  unsigned long flags;
  bool failed;
  spin_lock_irqsave(&ctx->adev->dev_lock, flags);
  failed = ctx->failed || acceldev_context_on_device_config_is_error(
                              ctx->adev->mgr->contexts[ctx->id].status);
  spin_unlock_irqrestore(&ctx->adev->dev_lock, flags);
  return failed;
}

static int acceldev_open(struct inode *inode, struct file *file) {
  struct acceldev_ctx *ctx;
  struct acceldev_dev *adev;
  int err;

  ctx = kzalloc(sizeof(*ctx), GFP_KERNEL);
  if (!ctx) {
    err = -ENOMEM;
    goto out_free_ctx;
  }
  adev = container_of(inode->i_cdev, struct acceldev_dev, cdev);
  ctx->adev = adev;
  if ((err = acceldev_ctxmgr_next_ctx(adev, &ctx))) {
    goto out_free_ctx;
  }

  ctx->buf_mgr = manager_init();
  if (!ctx->buf_mgr) {
    err = -ENOMEM;
    goto out_free_ctx;
  }

  file->private_data = ctx;
  return nonseekable_open(inode, file);

out_free_ctx:
  kfree(ctx);
  return err;
}

static int acceldev_close(struct inode *inode, struct file *file) {
  struct acceldev_ctx *ctx = file->private_data;

  accelde_ctxmgr_release_ctx(ctx->adev, ctx);
  manager_exit_ctx(ctx->buf_mgr, ctx);

  kfree(ctx);
  return 0;
}

static long acceldev_ioctl(struct file *file, unsigned int cmd,
                           unsigned long arg) {
  struct acceldev_ctx *ctx = file->private_data;
  if (acceldev_ctx_failed(ctx)) {
    return -EIO;
  }
  switch (cmd) {
  case ACCELDEV_IOCTL_CREATE_BUFFER:
    return acceldev_ioctl_create_buffer(ctx, arg);
  case ACCELDEV_IOCTL_RUN:
    return acceldev_ioctl_run(ctx, arg);
  case ACCELDEV_IOCTL_WAIT:
    return acceldev_ioctl_wait(ctx, arg);
  default:
    return -ENOTTY;
  }
}

const struct file_operations acceldev_fops = {
    .owner = THIS_MODULE,
    .open = acceldev_open,
    .release = acceldev_close,
    .unlocked_ioctl = acceldev_ioctl,
};
