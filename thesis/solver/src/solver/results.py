from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Mapping, Sequence

import torch


@dataclass(frozen=True)
class EquilibriumProfile:
    """Container for the candidate equilibrium pair."""

    player_one: torch.Tensor
    player_two: torch.Tensor
    scalar_relationship: float | None = None


@dataclass(frozen=True)
class SolverDiagnostics:
    """Structured metadata about the solve attempt."""

    converged: bool
    iterations: int
    stopping_reason: str
    history: Sequence[Mapping[str, torch.Tensor]] = field(default_factory=tuple)
    extras: Mapping[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class SolverResult:
    """Full record returned by solvers."""

    profile: EquilibriumProfile
    diagnostics: SolverDiagnostics
