from __future__ import annotations

from dataclasses import dataclass
from typing import Mapping

import torch

from ..base import EquilibriumSolver, SolverState
from ..config import SolverConfig
from ..results import EquilibriumProfile


@dataclass(frozen=True)
class GradientDescentHyperParams:
    """Parameter collection steering the gradient descent loop."""

    learning_rate: float = 1e-2
    momentum: float = 0.0
    gradient_clip: float | None = None
    project_after_step: bool = True
    line_search: bool = False

    @classmethod
    def from_config(cls, config: SolverConfig) -> "GradientDescentHyperParams":
        params: Mapping[str, float | bool | None] = config.algorithm_hyperparams
        return cls(
            learning_rate=float(params.get("learning_rate", cls.learning_rate)),
            momentum=float(params.get("momentum", cls.momentum)),
            gradient_clip=None
            if params.get("gradient_clip") is None
            else float(params["gradient_clip"]),
            project_after_step=bool(params.get("project_after_step", cls.project_after_step)),
            line_search=bool(params.get("line_search", cls.line_search)),
        )


class GradientDescentSolver(EquilibriumSolver):
    """Scaffold for a gradient-based equilibrium solver."""

    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.hyperparams = GradientDescentHyperParams.from_config(self.config)
        self._velocity_x: torch.Tensor | None = None
        self._velocity_y: torch.Tensor | None = None

    def initialize_profile(self) -> EquilibriumProfile:
        """Start from a neutral strategy pair until a better initializer is provided."""
        kwargs = self.runtime.as_torch_kwargs()
        decision_dim = self.game.decision_dim
        base_vector = torch.zeros(decision_dim, **kwargs)
        return EquilibriumProfile(
            player_one=base_vector.clone(),
            player_two=base_vector.clone(),
            scalar_relationship=1.0,
        )

    def compute_objective(self, profile: EquilibriumProfile) -> torch.Tensor:
        """Placeholder for the smooth objective used by the algorithm."""
        raise NotImplementedError("Objective definition depends on the finalized equilibrium characterization.")

    def compute_gradients(self, profile: EquilibriumProfile) -> tuple[torch.Tensor, torch.Tensor]:
        """Compute gradients with respect to each player's strategy."""
        raise NotImplementedError("Gradient derivation will use PyTorch autograd once the objective is fixed.")

    def apply_step(
        self,
        profile: EquilibriumProfile,
        gradients: tuple[torch.Tensor, torch.Tensor],
    ) -> EquilibriumProfile:
        """Update the strategies using gradient descent style dynamics."""
        grad_x, grad_y = gradients
        if self._velocity_x is None:
            self._velocity_x = torch.zeros_like(grad_x)
        if self._velocity_y is None:
            self._velocity_y = torch.zeros_like(grad_y)

        self._velocity_x.mul_(self.hyperparams.momentum).add_(grad_x)
        self._velocity_y.mul_(self.hyperparams.momentum).add_(grad_y)

        updated_x = profile.player_one - self.hyperparams.learning_rate * self._velocity_x
        updated_y = profile.player_two - self.hyperparams.learning_rate * self._velocity_y

        if self.hyperparams.gradient_clip is not None:
            limit = self.hyperparams.gradient_clip
            updated_x = torch.clamp(updated_x, -limit, limit)
            updated_y = torch.clamp(updated_y, -limit, limit)

        if self.hyperparams.project_after_step:
            updated_x = self.project(updated_x)
            updated_y = self.project(updated_y)

        return EquilibriumProfile(
            player_one=updated_x,
            player_two=updated_y,
            scalar_relationship=profile.scalar_relationship,
        )

    def project(self, vector: torch.Tensor) -> torch.Tensor:
        """Projection step keeps the strategy inside the feasible cone defined by the problem."""
        return torch.nn.functional.relu(vector)

    def step(self, state: SolverState) -> SolverState:
        """Execute one (placeholder) gradient descent iteration."""
        gradients = self.compute_gradients(state.profile)
        new_profile = self.apply_step(state.profile, gradients)

        objective_value = self.compute_objective(new_profile)
        self.record_metrics({"objective": objective_value})

        state.profile = new_profile
        state.converged = False
        state.stopping_reason = "pending-detailed-stopping-criteria"
        return state
