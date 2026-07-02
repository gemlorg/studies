#include "acceldev_driver.h"
#include <linux/kthread.h>

static int acceldev_executor_loop(void *);
enum executor_event { EVENT_RUN_NEXT, EVENT_FINISH, EVENT_NOP };

void acceldev_executor_init(struct acceldev_dev *adev) {
  struct acceldev_executor *executor = &adev->executor;
  INIT_LIST_HEAD(&executor->task_list);
  init_waitqueue_head(&executor->wq);
  executor->task =
      kthread_run(acceldev_executor_loop, adev, "acceldev_executor");
  if (IS_ERR(executor->task)) {
    pr_err("acceldev: executor thread creation failed\n");
    executor->task = NULL;
  }
}

void acceldev_executor_add_task(struct acceldev_ctx *ctx,
                                struct acceldev_task *task) {
  unsigned long flags;
  struct acceldev_dev *adev = ctx->adev;

  spin_lock_irqsave(&adev->dev_lock, flags);
  list_add_tail(&task->list, &adev->executor.task_list);
  spin_unlock_irqrestore(&adev->dev_lock, flags);
  wake_up_interruptible(&adev->executor.wq);
}

static enum executor_event executor_poll(struct acceldev_dev *adev) {
  unsigned long flags;

  if (kthread_should_stop()) {
    return EVENT_FINISH;
  }
  spin_lock_irqsave(&adev->dev_lock, flags);
  bool has_tasks = !list_empty(&adev->executor.task_list);
  spin_unlock_irqrestore(&adev->dev_lock, flags);
  if (has_tasks) {
    return EVENT_RUN_NEXT; // There are tasks to run
  } else {
    return EVENT_NOP; // No tasks to run
  }
}
static void executor_run(struct acceldev_dev *adev) {
  unsigned long flags;
  struct acceldev_task *task;

  spin_lock_irqsave(&adev->dev_lock, flags);
  if (list_empty(&adev->executor.task_list)) {
    spin_unlock_irqrestore(&adev->dev_lock, flags);
    return;
  }
  task =
      list_first_entry(&adev->executor.task_list, struct acceldev_task, list);
  list_del_init(&task->list);
  spin_unlock_irqrestore(&adev->dev_lock, flags);

  acceldev_submit_run(adev, task);
}

static int acceldev_executor_loop(void *adev_ptr) {
  enum executor_event next_event;
  struct acceldev_dev *adev = adev_ptr;

  while (!kthread_should_stop()) {
    next_event = executor_poll(adev);
    switch (next_event) {
    case EVENT_RUN_NEXT:
      executor_run(adev);
      continue;
    case EVENT_FINISH:
      continue;
    case EVENT_NOP:
      wait_event_interruptible(adev->executor.wq,
                               executor_poll(adev) != EVENT_NOP ||
                                   kthread_should_stop());
      continue;
    }
  }

  return 0;
}
