import torch
from torch import Tensor




def counts_to_config_ids(
    count_targets: Tensor,
    *,
    values_min: int = 2,  # Updated default to match dataset (1-9)
    values_max: int = 8,  # Updated default to match dataset (1-9)
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

    # 1. Identify which two shapes are active
    # We use topk to get indices of the two active shapes
    # values: [B, 2], indices: [B, 2]
    _, indices = torch.topk(counts, k=2, dim=1)

    # 2. Sort indices to ensure a < b (Canonical Pair Order)
    a = torch.min(indices, dim=1).values  # [B]
    b = torch.max(indices, dim=1).values  # [B]

    # 3. Compute Pair Index (Combinatorial Number System)
    # Maps unique pair (a, b) to a scalar index 0..14
    pair_index = a * (2 * n - a - 1) // 2 + (b - a - 1)  # [B]

    # 4. Determine 'Value' based on shape 'a'
    # FIX: We specifically take the count of shape 'a'.
    # This distinguishes "4 A, 6 B" (val=4) from "6 A, 4 B" (val=6).
    # We gather from the original counts tensor using index 'a'.
    v = torch.gather(counts, 1, a.unsqueeze(1)).squeeze(1)  # [B]

    # 5. Compute Offsets and Final ID
    num_values = values_max - values_min + 1
    value_offset = v - values_min  # [B]

    # Sanity check for range
    valid = (value_offset >= 0) & (value_offset < num_values)
    if not torch.all(valid):
        # Extract the invalid values for the error message
        bad_vals = v[~valid].tolist()
        raise ValueError(
            f"Found count values outside [{values_min}, {values_max}] in targets: {bad_vals}. "
            f"Check if values_min/max match your dataset."
        )

    config_ids = pair_index * num_values + value_offset
    return config_ids.to(dtype=torch.long, device=count_targets.device)