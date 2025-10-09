from __future__ import annotations

from abc import ABC, abstractmethod
from contextlib import contextmanager, nullcontext
from dataclasses import dataclass
from typing import ContextManager, Mapping, MutableSequence

import torch

from .config import SolverConfig, SolverRuntimeOptions
from .game import GameSpec
from .results import EquilibriumProfile, SolverDiagnostics, SolverResult


@dataclass
class SolverState:
    """Mutable container shared between solver iterations."""

    profile: EquilibriumProfile
    iteration: int = 0
    converged: bool = False
    stopping_reason: str | None = None


class EquilibriumSolver(ABC):
    """Base class for algorithms that compute equilibria for the investment game."""

    def __init__(
        self,
        game: GameSpec,
        config: SolverConfig | None = None,
        runtime: SolverRuntimeOptions | None = None,
    ) -> None:
        self.game = game.ensure_valid()
        self.config = config or SolverConfig()
        self.runtime = runtime or SolverRuntimeOptions()
        self._history: MutableSequence[Mapping[str, torch.Tensor]] = []

    def reset_history(self) -> None:
        """Clear accumulated trace data."""
        self._history.clear()

    def record_metrics(self, metrics: Mapping[str, torch.Tensor]) -> None:
        """Store iteration metrics if tracking is enabled."""
        if not self.config.track_history:
            return
        snapshot = {key: value.detach().clone() for key, value in metrics.items()}
        self._history.append(snapshot)

    @contextmanager
    def autocast_context(self) -> ContextManager[None]:
        """Context manager for optional autocasting."""
        if self.runtime.autocast_precision is None:
            with nullcontext():
                yield
            return
        device_type = self.runtime.device.type if self.runtime.device else "cpu"
        with torch.autocast(device_type=device_type, dtype=self.runtime.autocast_precision):
            yield

    @abstractmethod
    def initialize_profile(self) -> EquilibriumProfile:
        """Select a feasible starting point for both players."""

    @abstractmethod
    def step(self, state: SolverState) -> SolverState:
        """Perform a single iteration and update the shared state."""

    def finalize(self, state: SolverState) -> SolverResult:
        """Assemble the public result from the final state."""
        diagnostics = SolverDiagnostics(
            converged=state.converged,
            iterations=state.iteration,
            stopping_reason=state.stopping_reason or ("converged" if state.converged else "unspecified"),
            history=tuple(self._history),
        )
        return SolverResult(profile=state.profile, diagnostics=diagnostics)

    def solve(self) -> SolverResult:
        """Run the solver loop until convergence or stopping."""
        with self.autocast_context():
            state = SolverState(profile=self.initialize_profile())
            for iteration in range(1, self.config.max_iterations + 1):
                state.iteration = iteration
                state = self.step(state)
                if state.converged:
                    state.stopping_reason = state.stopping_reason or "tolerance"
                    break
            else:
                state.stopping_reason = state.stopping_reason or "max_iterations"
        return self.finalize(state)
