import torch
from torch import Tensor




def counts_to_config_ids(
    count_targets: Tensor,
    *,
    values_min: int = 1,
    values_max: int = 9,
) -> Tensor:
    """
    Map per-sample 6D counts -> scalar configuration IDs.

    Args
    ----
    count_targets:
        Tensor of shape [B, 6].
        Expected: exactly two entries per row are > 0.
    
    Returns
    -------
    config_ids:
        Long tensor of shape [B] with configuration IDs.
    """
    if count_targets.ndim != 2 or count_targets.size(1) != 6:
        raise ValueError(
            f"Expected count_targets of shape [B, 6], got {tuple(count_targets.shape)}"
        )

    # Ensure we are working with long for indexing/values
    counts = count_targets.to(torch.long)
    B, n = counts.shape  # n should be 6

    # identify which two shapes are active
    _, indices = torch.topk(counts, k=2, dim=1)

    # sort indices to ensure a < b (Canonical Pair Order)
    a = torch.min(indices, dim=1).values  # [B]
    b = torch.max(indices, dim=1).values  # [B]

    # compute pair index in 0..14
    pair_index = a * (2 * n - a - 1) // 2 + (b - a - 1)  # [B]
    
    # get the count for for a 
    v = counts[torch.arange(B), a]

    # compute count id
    num_values = values_max - values_min + 1
    value_offset = v - values_min  # [B]

    valid = (value_offset >= 0) & (value_offset < num_values)
    if not torch.all(valid):
        bad_vals = v[~valid].tolist()
        raise ValueError(
            f"Found count values outside [{values_min}, {values_max}] in targets: {bad_vals}. "
            f"Check if values_min/max match your dataset."
        )

    config_ids = pair_index * num_values + value_offset
    return config_ids.to(dtype=torch.long, device=count_targets.device)
