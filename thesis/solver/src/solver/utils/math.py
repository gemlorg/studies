from __future__ import annotations

import torch


def tensor_gcd(values: torch.Tensor) -> torch.Tensor:
    """Compute the GCD across a tensor of integer-like values.

    The input is promoted to ``torch.int64`` and absolute-valued before
    aggregation. A scalar zero tensor is returned for empty inputs.
    """
    if values.numel() == 0:
        return torch.zeros((), dtype=torch.int64)

    integers = values.to(torch.int64).abs()
    gcd_value = integers[0]
    for idx in range(1, integers.numel()):
        gcd_value = torch.gcd(gcd_value, integers[idx])
    return gcd_value
