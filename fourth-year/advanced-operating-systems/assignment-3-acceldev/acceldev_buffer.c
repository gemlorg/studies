#include "acceldev_driver.h"

static vm_fault_t acceldev_vma_fault(struct vm_fault *vmf) {
  struct acceldev_pt *pt = vmf->vma->vm_private_data;
  if (vmf->pgoff >= pt->num_pages)
    return VM_FAULT_SIGBUS;
  vmf->page = virt_to_page(pt->pages[vmf->pgoff].kern);
  get_page(vmf->page);
  return 0;
}

static const struct vm_operations_struct acceldev_vm_ops = {
    .fault = acceldev_vma_fault,
};

static int acceldev_buffer_release(struct inode *inode, struct file *file) {
  struct acceldev_buffer *buf = file->private_data;
  struct acceldev_pt *pt = buf->pt;
  manager_unbind_slot(buf->buf_mgr, buf);
  drop_acceldev_pt(pt);
  kfree(pt);
  kfree(buf);
  return 0;
}

static int acceldev_mmap(struct file *file, struct vm_area_struct *vma) {
  struct acceldev_buffer *buf = file->private_data;
  unsigned long length = vma->vm_end - vma->vm_start;
  if (!(vma->vm_flags & VM_SHARED)) {
    return -EINVAL;
  }

  vma->vm_ops = &acceldev_vm_ops;
  vma->vm_private_data = buf->pt;
  return 0;
}

const struct file_operations acceldev_buffer_fops = {
    .owner = THIS_MODULE,
    .mmap = acceldev_mmap,
    .release = acceldev_buffer_release,
};
