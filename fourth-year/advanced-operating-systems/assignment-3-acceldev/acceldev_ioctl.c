#include "acceldev_driver.h"

#define BUFFER_FILE_FLAGS O_RDWR

long acceldev_ioctl_create_buffer(struct acceldev_ctx *ctx, unsigned long arg) {
  struct acceldev_ioctl_create_buffer user;
  struct acceldev_ioctl_create_buffer_result result;
  struct acceldev_buffer *buf;
  struct file *fil;
  int fd, err;

  if (copy_from_user(&user, (void __user *)arg, sizeof(user))) {
    err = -EFAULT;
    goto fail_arg;
  }
  if (user.size <= 0 || user.size > ACCELDEV_BUFFER_MAX_SIZE) {
    err = -EINVAL;
    goto fail_arg;
  }
  if (user.type < 0 || user.type >= BUFFER_TYPES_COUNT) {
    err = -EINVAL;
    goto fail_arg;
  }

  buf = kzalloc(sizeof(*buf), GFP_KERNEL);
  if (!buf) {
    err = -ENOMEM;
    goto fail_arg;
  }
  buf->id = ACCELDEV_INVALID_SLOT;
  buf->pt = kzalloc(sizeof(*buf->pt), GFP_KERNEL);
  if (!buf->pt) {
    err = -ENOMEM;
    goto fail_buf;
  }
  buf->size = ALIGN(user.size, ACCELDEV_PAGE_SIZE);
  if ((err = init_acceldev_pt(buf->pt, buf->size, &ctx->adev->pdev->dev))) {
    goto fail_pt_alloc;
  }

  buf->type = user.type;
  buf->ctx = ctx;
  buf->buf_mgr = ctx->buf_mgr;

  fd = get_unused_fd_flags(BUFFER_FILE_FLAGS);
  if (fd < 0) {
    err = fd;
    goto fail_pt;
  }

  fil = anon_inode_getfile("acceldev-buffer", &acceldev_buffer_fops, buf,
                           BUFFER_FILE_FLAGS);
  if (IS_ERR(fil)) {
    err = PTR_ERR(fil);
    goto fail_putfd;
  }

  if ((err = manager_bind_slot(ctx->buf_mgr, buf))) {
    goto fail_putfd;
  }

  fil->private_data = buf;
  fd_install(fd, fil);

  result.buffer_slot = buf->id;

  if (copy_to_user(user.result, &result, sizeof(result))) {
    err = -EFAULT;
    goto fail_resput;
  }

  return fd;

fail_resput:
  fput(fil);
fail_putfd:
  put_unused_fd(fd);
fail_pt:
  drop_acceldev_pt(buf->pt);
fail_pt_alloc:
  kfree(buf->pt);
fail_buf:
  kfree(buf);
fail_arg:
  acceldev_fail_ctx(ctx);
  return err;
}

static struct acceldev_task *
acceldev_task_create_submit(struct acceldev_ctx *ctx,
                            struct acceldev_buffer *buf, uint32_t offset,
                            uint32_t size) {
  struct acceldev_task *task;
  task = kzalloc(sizeof(*task), GFP_KERNEL);
  if (!task)
    return ERR_PTR(-ENOMEM);
  task->buf = buf;
  task->offset = offset;
  task->size = size;
  acceldev_executor_add_task(ctx, task);
  return task;
}

static int analyze_user_cmds(struct acceldev_pt *pt, uint32_t addr,
                             uint32_t size) {
  uint32_t cmd_size = ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t);

  if (addr % cmd_size != 0 || size % cmd_size != 0) {
    return -EINVAL;
  }

  return 0;
}
long acceldev_ioctl_run(struct acceldev_ctx *ctx, unsigned long arg) {
  struct acceldev_ioctl_run user;
  struct fd f;
  struct file *filp;
  struct acceldev_buffer *buf;
  int err, cmd_size;
  if (copy_from_user(&user, (void __user *)arg, sizeof(user))) {
    err = -EFAULT;
    goto err_arg;
  }

  cmd_size = ACCELDEV_USER_CMD_WORDS * sizeof(uint32_t);

  user.size += (cmd_size - user.size % cmd_size) % cmd_size;
  f = fdget(user.cfd);
  filp = fd_file(f);
  if (!filp || filp->f_op != &acceldev_buffer_fops) {
    err = -EINVAL;
    goto err_fdput;
  }
  buf = filp->private_data;
  if (buf->ctx != ctx || buf->type != BUFFER_TYPE_CODE) {
    err = -EINVAL;
    goto err_fdput;
  }
  if (analyze_user_cmds(buf->pt, user.addr, user.size)) {
    err = -EINVAL;
    goto err_fdput;
  }
  acceldev_task_create_submit(ctx, buf, user.addr, user.size);
  fdput(f);
  return 0;

err_fdput:
  fdput(f);
err_arg:
  acceldev_fail_ctx(ctx);
  return err;
}

long acceldev_ioctl_wait(struct acceldev_ctx *ctx, unsigned long arg) {
  struct acceldev_ioctl_wait user;
  struct acceldev_dev *adev;
  long ret;

  if (copy_from_user(&user, (void __user *)arg, sizeof(user))) {
    return -EFAULT;
  }

  adev = ctx->adev;

  ret = wait_event_interruptible(
      adev->fence_queue,
      user.fence_wait <= ctx->adev->mgr->contexts[ctx->id].fence_counter ||
          acceldev_ctx_failed(ctx));

  if (ret) {
    return ret;
  }

  if (acceldev_ctx_failed(ctx))
    return -EIO;
  return 0;
}
