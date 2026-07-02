#include "acceldev_driver.h"

dev_t acceldev_devt;
struct class acceldev_class = {
    .name = ACCELDEV_NAME,
};

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Heorhii Lopatin");

static int __init acceldev_init(void) {
  int err;

  err = alloc_chrdev_region(&acceldev_devt, 0, ACCELDEV_MAX_DEVICES,
                            ACCELDEV_NAME);
  if (err)
    goto err_alloc;
  if ((err = class_register(&acceldev_class)))
    goto err_class;
  if ((err = pci_register_driver(&acceldev_driver)))
    goto err_pci;
  return 0;

err_pci:
  class_unregister(&acceldev_class);
err_class:
  unregister_chrdev_region(acceldev_devt, ACCELDEV_MAX_DEVICES);
err_alloc:
  return err;
}

static void __exit acceldev_exit(void) {
  pci_unregister_driver(&acceldev_driver);
  class_unregister(&acceldev_class);
  unregister_chrdev_region(acceldev_devt, ACCELDEV_MAX_DEVICES);
}

module_init(acceldev_init);
module_exit(acceldev_exit);
