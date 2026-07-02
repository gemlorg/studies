#include "acceldev_driver.h"

#define MAKE_PT_ENTRY(addr) (0x1 | ((addr) >> 8))

struct acceldev_buffer_manager *manager_init(void) {
  struct acceldev_buffer_manager *mgr;
  int i;

  mgr = kzalloc(sizeof(*mgr), GFP_KERNEL);
  if (!mgr) {
    return NULL;
  }
  spin_lock_init(&mgr->lock);
  mgr->num_free_slots = ACCELDEV_NUM_BUFFERS;
  mgr->ctx_exited = false;
  for (i = 0; i < ACCELDEV_NUM_BUFFERS; i++) {
    mgr->context_slots[i] = NULL;
  }
  return mgr;
}

static void manager_release_if_free(struct acceldev_buffer_manager *mgr) {

  if (!mgr)
    return;
  if (!mgr->ctx_exited)
    return;

  if (mgr->num_free_slots != ACCELDEV_NUM_BUFFERS) {
    return;
  }
  kfree(mgr);
}

void manager_exit_ctx(struct acceldev_buffer_manager *mgr,
                      struct acceldev_ctx *ctx) {
  mgr->ctx_exited = true;
  int i;
  for (i = 0; i < ACCELDEV_NUM_BUFFERS; i++) {
    if (mgr->context_slots[i]) {
      uint32_t cmd[ACCELDEV_DEVICE_CMD_WORDS];
      cmd[0] = ACCELDEV_DEVICE_CMD_BIND_SLOT_HEADER(ctx->id);
      cmd[1] = i;
      cmd[2] = 0;
      cmd[3] = 0;
      cmd[4] = 0;
      acceldev_feed_cmd(ctx->adev, cmd);
    }
  }
  manager_release_if_free(mgr);
}

int manager_bind_slot(struct acceldev_buffer_manager *mgr,
                      struct acceldev_buffer *buf) {
  int i;
  int slot = -1, err = 0;
  unsigned long flags;
  if (buf->type != BUFFER_TYPE_DATA)
    return 0;
  spin_lock_irqsave(&mgr->lock, flags);
  if (mgr->num_free_slots <= 0) {
    err = -ENOSPC;
    goto out_unlock;
  }
  if (mgr->ctx_exited) {
    err = -EINVAL;
    goto out_unlock;
  }
  for (i = 0; i < ACCELDEV_NUM_BUFFERS; i++) {
    if (mgr->context_slots[i] == NULL) {
      slot = i;
      mgr->context_slots[i] = buf;
      mgr->num_free_slots--;
      break;
    }
  }
  if (slot < 0) {
    err = -ENOSPC;
    goto out_unlock;
  }
  buf->id = slot;
out_unlock:
  spin_unlock_irqrestore(&mgr->lock, flags);
  if (!err)
    acceldev_submit_bind_slot(buf);
  return err;
}
void manager_unbind_slot(struct acceldev_buffer_manager *mgr,
                         struct acceldev_buffer *buf) {
  if (!buf || !mgr)
    return;
  int slot = buf->id;
  unsigned long flags;
  if (buf->type != BUFFER_TYPE_DATA)
    return;
  if (slot < 0 || slot >= ACCELDEV_NUM_BUFFERS)
    return;
  spin_lock_irqsave(&mgr->lock, flags);
  if (mgr->context_slots[slot] != buf) {
    spin_unlock_irqrestore(&mgr->lock, flags);
    return;
  }
  mgr->context_slots[slot] = NULL;
  mgr->num_free_slots++;
  spin_unlock_irqrestore(&mgr->lock, flags);
  if (!mgr->ctx_exited)
    acceldev_submit_unbind_slot(buf);
  manager_release_if_free(mgr);
}

static inline int get_num_pages(size_t size) {
  return DIV_ROUND_UP(size, PAGE_SIZE);
}

static inline void *dma_zalloc_page(struct device *dev,
                                    dma_addr_t *dma_handle) {
  return dma_alloc_coherent(dev, ACCELDEV_PAGE_SIZE, dma_handle,
                            GFP_KERNEL | __GFP_ZERO);
}

static inline void dma_free_page(struct device *dev, void *cpu_addr,
                                 dma_addr_t dma_handle) {
  dma_free_coherent(dev, PAGE_SIZE, cpu_addr, dma_handle);
}

int init_acceldev_pt(struct acceldev_pt *pt, size_t size, struct device *dev) {
  int i, num_pages;
  uint32_t *page_table;

  num_pages = get_num_pages(size);

  pt->pages = kcalloc(num_pages, sizeof(*pt->pages), GFP_KERNEL);
  if (!pt->pages)
    return -ENOMEM;
  for (i = 0; i < num_pages; i++) {
    pt->pages[i].kern = dma_zalloc_page(dev, &pt->pages[i].dev);
    if (!pt->pages[i].kern)
      goto alloc_err;
  }
  pt->num_pages = num_pages;
  pt->dev = dev;
  pt->pt.kern = dma_zalloc_page(dev, &pt->pt.dev);
  if (!pt->pt.kern)
    goto alloc_err;
  page_table = (uint32_t *)pt->pt.kern;

  for (i = 0; i < num_pages; i++) {
    page_table[i] = MAKE_PT_ENTRY(pt->pages[i].dev);
  }
  return 0;

alloc_err:
  while (--i >= 0)
    dma_free_page(dev, pt->pages[i].kern, pt->pages[i].dev);
  kfree(pt->pages);
  return -ENOMEM;
}

void drop_acceldev_pt(struct acceldev_pt *pt) {
  int i;
  if (pt->pt.kern)
    dma_free_page(pt->dev, pt->pt.kern, pt->pt.dev);
  for (i = 0; i < pt->num_pages; i++)
    dma_free_page(pt->dev, pt->pages[i].kern, pt->pages[i].dev);
  kfree(pt->pages);
}

int acceldev_ctxmgr_init(struct acceldev_dev *adev) {

  struct acceldev_context_manager *mgr =
      kzalloc(sizeof(struct acceldev_context_manager), GFP_KERNEL);
  adev->mgr = mgr;
  if (!mgr) {
    return -ENOMEM;
  }

  adev->mgr->contexts = dma_alloc_coherent(
      &adev->pdev->dev, ACCELDEV_MAX_CONTEXTS * sizeof(*adev->mgr->contexts),
      &adev->mgr->contexts_dma, GFP_KERNEL);
  if (!adev->mgr->contexts) {
    kfree(mgr);
    return -ENOMEM;
  }
  return 0;
}
void acceldev_ctxmgr_fail_contexts(struct acceldev_dev *adev) {
  int i;
  for (i = 0; i < ACCELDEV_MAX_CONTEXTS; i++) {
    struct acceldev_ctx *ctx = adev->mgr->ctx_refs[i];
    if (ctx) {
      acceldev_fail_ctx(ctx);
      ctx->id = -1;
      adev->mgr->ctx_refs[i] = NULL;
    }
  }
  memset(adev->mgr->contexts, 0,
         ACCELDEV_MAX_CONTEXTS * sizeof(*adev->mgr->contexts));
}

int acceldev_ctxmgr_next_ctx(struct acceldev_dev *adev,
                             struct acceldev_ctx **ctx) {
  int i = 0;
  for (i = 0; i < ACCELDEV_MAX_CONTEXTS; i++) {
    if (adev->mgr->ctx_refs[i] == NULL) {
      break;
    }
  }
  if (i >= ACCELDEV_MAX_CONTEXTS) {
    return -ENOSPC;
  }
  adev->mgr->ctx_refs[i] = *ctx;
  (*ctx)->id = i;
  return 0;
}
void accelde_ctxmgr_release_ctx(struct acceldev_dev *adev,
                                struct acceldev_ctx *ctx) {
  if (ctx->id < 0 || ctx->id >= ACCELDEV_MAX_CONTEXTS) {
    return;
  }

  adev->mgr->ctx_refs[ctx->id] = NULL;
  memset(&adev->mgr->contexts[ctx->id], 0,
         sizeof(struct acceldev_context_on_device_config));
}
void release_ctxmgr(struct acceldev_dev *adev) {
  if (adev->mgr) {
    dma_free_coherent(&adev->pdev->dev,
                      ACCELDEV_MAX_CONTEXTS * sizeof(*adev->mgr->contexts),
                      adev->mgr->contexts, adev->mgr->contexts_dma);
    kfree(adev->mgr);
    adev->mgr = NULL;
  }
}
