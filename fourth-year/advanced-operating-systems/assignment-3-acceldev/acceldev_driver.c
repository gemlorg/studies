#include "acceldev_driver.h"

static struct acceldev_dev *acceldev_devices[ACCELDEV_MAX_DEVICES];
static DEFINE_MUTEX(acceldev_devices_lock);

static irqreturn_t acceldev_irq_handler(int irq, void *dev_ptr) {
  struct acceldev_dev *adev = dev_ptr;
  uint32_t status;
  status = acceldev_ior(adev, ACCELDEV_INTR) &
           acceldev_ior(adev, ACCELDEV_INTR_ENABLE);
  if (!status)
    goto out;

  acceldev_iow(adev, ACCELDEV_INTR, status);
  if (status & (ACCELDEV_INTR_USER_FENCE_WAIT | ACCELDEV_INTR_FENCE_WAIT)) {
    wake_up_interruptible_all(&adev->fence_queue);
  }

  if (status & (ACCELDEV_INTR_CMD_ERROR | ACCELDEV_INTR_MEM_ERROR)) {
    wake_up_interruptible_all(&adev->fence_queue);
    acceldev_restart_device(adev);
  }

out:
  return IRQ_RETVAL(status);
}

static int find_slot(struct acceldev_dev *adev) {
  int idx;

  mutex_lock(&acceldev_devices_lock);
  for (idx = 0; idx < ACCELDEV_MAX_DEVICES; idx++) {
    if (!acceldev_devices[idx]) {
      acceldev_devices[idx] = adev;
      adev->idx = idx;
      mutex_unlock(&acceldev_devices_lock);
      return 0;
    }
  }
  mutex_unlock(&acceldev_devices_lock);
  return -ENODEV;
}
static void free_slot(struct acceldev_dev *pdev) {

  mutex_lock(&acceldev_devices_lock);
  if (pdev->idx >= 0 && pdev->idx < ACCELDEV_MAX_DEVICES) {
    acceldev_devices[pdev->idx] = NULL;
    pdev->idx = -1;
  }
  mutex_unlock(&acceldev_devices_lock);
}

static int acceldev_probe(struct pci_dev *pdev,
                          const struct pci_device_id *id) {
  int err;
  struct acceldev_dev *adev;

  adev = kzalloc(sizeof(*adev), GFP_KERNEL);
  if (!adev) {
    err = -ENOMEM;
    goto err_alloc;
  }

  spin_lock_init(&adev->dev_lock);
  init_waitqueue_head(&adev->fence_queue);
  acceldev_executor_init(adev);
  pci_set_drvdata(pdev, adev);
  adev->pdev = pdev;

  if ((err = find_slot(adev))) {
    goto err_no_slot;
  }
  err = pci_enable_device(pdev);
  if (err)
    goto err_enable;

  pci_set_master(pdev);

  if ((err = pci_request_regions(pdev, ACCELDEV_NAME))) {
    goto err_regions;
  }
  adev->bar = pci_iomap(pdev, 0, ACCELDEV_BAR_SIZE);
  if (!adev->bar) {
    err = -ENOMEM;
    goto err_iomap;
  }
  err = request_irq(pdev->irq, acceldev_irq_handler, IRQF_SHARED, ACCELDEV_NAME,
                    adev);
  if (err)
    goto err_irq;

  err = acceldev_ctxmgr_init(adev);
  if (err)
    goto err_ctxmgr;
  start_device(adev);

  cdev_init(&adev->cdev, &acceldev_fops);
  adev->cdev.owner = THIS_MODULE;
  err = cdev_add(&adev->cdev, acceldev_devt + adev->idx, 1);
  if (err)
    goto err_cdev;

  adev->dev =
      device_create(&acceldev_class, &pdev->dev, acceldev_devt + adev->idx,
                    NULL, ACCELDEV_NAME "%d", adev->idx);
  if (IS_ERR(adev->dev)) {
    err = PTR_ERR(adev->dev);
    goto err_dev;
  }

  return 0;

err_dev:
  cdev_del(&adev->cdev);
err_cdev:
  release_ctxmgr(adev);
err_ctxmgr:
  free_irq(pdev->irq, adev);
err_irq:
  pci_iounmap(pdev, adev->bar);
err_iomap:
  pci_release_regions(pdev);
err_regions:
  pci_disable_device(pdev);
err_enable:
  free_slot(adev);
err_no_slot:
  pci_set_drvdata(pdev, NULL);
  kfree(adev);
err_alloc:
  return err;
}

static void acceldev_remove(struct pci_dev *pdev) {
  struct acceldev_dev *adev = pci_get_drvdata(pdev);
  int idx = adev->idx;
  acceldev_iow(adev, ACCELDEV_INTR, 0);
  acceldev_iow(adev, ACCELDEV_ENABLE, 0);
  if (adev->executor.task && !IS_ERR(adev->executor.task))
    kthread_stop(adev->executor.task);

  device_destroy(&acceldev_class, acceldev_devt + idx);
  cdev_del(&adev->cdev);

  free_irq(pdev->irq, adev);
  pci_iounmap(pdev, adev->bar);

  pci_release_regions(pdev);
  pci_disable_device(pdev);

  free_slot(adev);
  pci_set_drvdata(pdev, NULL);
  release_ctxmgr(adev);

  kfree(adev);
}

static const struct pci_device_id acceldev_ids[] = {
    {PCI_DEVICE(ACCELDEV_VENDOR_ID, ACCELDEV_DEVICE_ID)}, {}};
MODULE_DEVICE_TABLE(pci, acceldev_ids);

struct pci_driver acceldev_driver = {
    .name = ACCELDEV_NAME,
    .id_table = acceldev_ids,
    .probe = acceldev_probe,
    .remove = acceldev_remove,
};
