from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Mapping, MutableMapping

import torch


@dataclass(frozen=True)
class SolverConfig:
    """High-level configuration shared by solver implementations."""

    max_iterations: int = 50_000
    tolerance: float = 1e-6
    track_history: bool = True
    random_seed: int | None = None
    algorithm_hyperparams: Mapping[str, Any] = field(default_factory=dict)

    def extend(self, **overrides: Any) -> "SolverConfig":
        """Return an updated copy with algorithm-specific overrides."""
        data: dict[str, Any] = {
            "max_iterations": self.max_iterations,
            "tolerance": self.tolerance,
            "track_history": self.track_history,
            "random_seed": self.random_seed,
            "algorithm_hyperparams": dict(self.algorithm_hyperparams),
        }
        data.update(overrides)
        return SolverConfig(**data)


@dataclass(frozen=True)
class SolverRuntimeOptions:
    """Runtime knobs that do not affect the recorded configuration."""

    device: torch.device | None = None
    dtype: torch.dtype | None = None
    autocast_precision: torch.dtype | None = None

    def as_torch_kwargs(self) -> MutableMapping[str, Any]:
        """Translate runtime options into torch.tensor keyword arguments."""
        kwargs: dict[str, Any] = {}
        if self.device is not None:
            kwargs["device"] = self.device
        if self.dtype is not None:
            kwargs["dtype"] = self.dtype
        return kwargs
