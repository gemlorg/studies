from __future__ import annotations

import torch

from solver.game import TullockContestGame


def build_identity_tullock_game(
    dimension: int,
    lambda_param: float = 0.5,
    *,
    dtype: torch.dtype = torch.float32,
    device: torch.device | None = None,
) -> TullockContestGame:
    """Construct a Tullock game with identity payoff matrix and linear field values."""
    if dimension <= 0:
        raise ValueError("dimension must be positive.")

    matrix = torch.eye(dimension, dtype=dtype, device=device)
    field_values = torch.arange(1, dimension + 1, dtype=dtype, device=device)
    investment_costs = torch.tensor([1.0, 2.0], dtype=dtype, device=device)
    return TullockContestGame(
        matrix=matrix,
        investment_costs=investment_costs,
        field_values=field_values,
        lambda_param=lambda_param,
    )
