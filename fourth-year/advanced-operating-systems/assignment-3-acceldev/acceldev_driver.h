#ifndef ACCELDEV_DRIVER_H
#define ACCELDEV_DRIVER_H

#include "acceldev.h"
#include <linux/anon_inodes.h>
#include <linux/atomic.h>
#include <linux/cdev.h>
#include <linux/delay.h>
#include <linux/device.h>
#include <linux/dma-mapping.h>
#include <linux/fd.h>
#include <linux/file.h>
#include <linux/fs.h>
#include <linux/interrupt.h>
#include <linux/irqreturn.h>
#include <linux/kthread.h>
#include <linux/list.h>
#include <linux/mm.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/pci.h>
#include <linux/printk.h>
#include <linux/slab.h>
#include <linux/spinlock.h>
#include <linux/string.h>
#include <linux/types.h>
#include <linux/uaccess.h>
#include <linux/wait.h>

#define ACCELDEV_INVALID_SLOT ACCELDEV_NUM_BUFFERS
#define ACCELDEV_MAX_DEVICES 2

extern struct pci_driver acceldev_driver;
extern const struct file_operations acceldev_fops;
extern const struct file_operations acceldev_buffer_fops;

extern dev_t acceldev_devt;
extern struct class acceldev_class;

struct acceldev_page {
  void *kern;
  dma_addr_t dev;
};

struct acceldev_pt {
  struct acceldev_page pt;
  int num_pages;
  struct acceldev_page *pages;
  struct device *dev;
};

struct acceldev_buffer_manager;

struct acceldev_buffer {
  enum acceldev_buffer_type type;
  size_t size;
  struct acceldev_ctx *ctx;
  struct acceldev_pt *pt;
  int id;
  struct acceldev_buffer_manager *buf_mgr;
};

struct acceldev_task {
  struct list_head list;
  struct acceldev_buffer *buf;
  uint32_t offset;
  uint32_t size;
};

struct acceldev_buffer_manager {
  spinlock_t lock;
  struct acceldev_buffer *context_slots[ACCELDEV_NUM_BUFFERS];
  int num_free_slots;
  bool ctx_exited;
};

struct acceldev_executor {
  struct task_struct *task;   /* Executor task */
  struct list_head task_list; /* List of pending tasks */
  wait_queue_head_t wq;
};

struct acceldev_context_manager {
  struct acceldev_context_on_device_config *contexts;
  dma_addr_t contexts_dma;
  struct acceldev_ctx *ctx_refs[ACCELDEV_MAX_CONTEXTS];
};

struct acceldev_dev {
  struct pci_dev *pdev;
  struct cdev cdev;
  struct device *dev;
  void __iomem *bar;

  int idx;
  spinlock_t dev_lock;
  wait_queue_head_t fence_queue;

  struct acceldev_context_manager *mgr;
  struct acceldev_executor executor;
};

struct acceldev_ctx {
  int id;
  bool failed;
  struct acceldev_buffer_manager *buf_mgr;
  struct acceldev_dev *adev;
};

/* IOCTL command handlers */
long acceldev_ioctl_create_buffer(struct acceldev_ctx *ctx, unsigned long arg);
long acceldev_ioctl_run(struct acceldev_ctx *ctx, unsigned long arg);
long acceldev_ioctl_wait(struct acceldev_ctx *ctx, unsigned long arg);

// Buffer manager
struct acceldev_buffer_manager *manager_init(void);
void manager_exit_ctx(struct acceldev_buffer_manager *mgr,
                      struct acceldev_ctx *ctx);
int manager_bind_slot(struct acceldev_buffer_manager *mgr,
                      struct acceldev_buffer *buf);
void manager_unbind_slot(struct acceldev_buffer_manager *mgr,
                         struct acceldev_buffer *buf);

// ctx manager

int acceldev_ctxmgr_init(struct acceldev_dev *dev);
void acceldev_ctxmgr_fail_contexts(struct acceldev_dev *adev);
int acceldev_ctxmgr_next_ctx(struct acceldev_dev *adev,
                             struct acceldev_ctx **ctx);
void accelde_ctxmgr_release_ctx(struct acceldev_dev *adev,
                                struct acceldev_ctx *ctx);

// Utils
int init_acceldev_pt(struct acceldev_pt *pt, size_t size, struct device *dev);
void drop_acceldev_pt(struct acceldev_pt *pt);
void start_device(struct acceldev_dev *dev);
void acceldev_restart_device(struct acceldev_dev *dev);

void release_ctxmgr(struct acceldev_dev *adev);

bool acceldev_ctx_failed(struct acceldev_ctx *ctx);
void acceldev_fail_ctx(struct acceldev_ctx *ctx);

void acceldev_iow(struct acceldev_dev *dev, uint32_t reg, uint32_t val);
void acceldev_feed_cmd(struct acceldev_dev *dev, uint32_t *cmd_words);
uint32_t acceldev_ior(struct acceldev_dev *dev, uint32_t reg);

// cmds
void acceldev_submit_bind_slot(struct acceldev_buffer *buf);
void acceldev_submit_unbind_slot(struct acceldev_buffer *buf);
void acceldev_submit_run(struct acceldev_dev *adev, struct acceldev_task *task);

// executor
void acceldev_executor_init(struct acceldev_dev *adev);
void acceldev_executor_add_task(struct acceldev_ctx *ctx,
                                struct acceldev_task *task);

struct acceldev_task *acceldev_task_create(struct acceldev_ctx *ctx,
                                           struct acceldev_buffer *buf,
                                           uint32_t offset, uint32_t size);
#endif /* ACCELDEV_DRIVER_H */
