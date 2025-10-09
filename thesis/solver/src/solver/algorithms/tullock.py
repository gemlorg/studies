from __future__ import annotations

from typing import cast

import torch

from ..game import TullockContestGame
from ..results import EquilibriumProfile
from ..utils import tensor_gcd
from .gradient_descent import GradientDescentSolver


class TullockGradientDescentSolver(GradientDescentSolver):
    """Gradient-based solver restricted to Tullock contest success functions."""

    game: TullockContestGame

    def __init__(self, game: TullockContestGame, *args, **kwargs) -> None:
        if not isinstance(game, TullockContestGame):
            raise TypeError("TullockGradientDescentSolver requires a TullockContestGame instance.")
        super().__init__(game, *args, **kwargs)
        self.game = cast(TullockContestGame, self.game)
        self._x_state = self._sample_initial_x()

    def _sample_initial_x(self) -> torch.Tensor:
        kwargs = self.runtime.as_torch_kwargs()
        rand_kwargs = dict(kwargs)
        generator = None
        device = kwargs.get("device")
        if self.config.random_seed is not None:
            generator_device = device if isinstance(device, torch.device) else torch.device(device) if device else torch.device("cpu")
            if generator_device.type == "cpu":
                generator = torch.Generator(device="cpu")
                generator.manual_seed(self.config.random_seed)
        if generator is not None:
            rand_kwargs["generator"] = generator
        base = torch.rand(self.game.field_count, **rand_kwargs)
        return torch.nn.functional.relu(base)

    def initialize_profile(self) -> EquilibriumProfile:
        strategies = self.game.strategies_from_x(self._x_state)
        support = self._support_indices(self._x_state)
        self._record_support_metadata(support)
        scaling_ratio = self._scalar_relationship()
        return EquilibriumProfile(
            player_one=strategies[0],
            player_two=strategies[1],
            scalar_relationship=scaling_ratio,
        )

    def compute_objective(self, profile: EquilibriumProfile) -> torch.Tensor:
        x = self.game.recover_x((profile.player_one, profile.player_two))
        constraint_violation = self.game.constraint_violation(x)
        violation_penalty = torch.nn.functional.relu(constraint_violation).pow(2).mean()
        payoffs = self.game.expected_payoffs((profile.player_one, profile.player_two))
        total_payoff = torch.stack(payoffs).mean()
        return violation_penalty - total_payoff

    def loss_from_x(self, x: torch.Tensor) -> torch.Tensor:
        strategies = self.game.strategies_from_x(x)
        profile = EquilibriumProfile(
            player_one=strategies[0],
            player_two=strategies[1],
            scalar_relationship=self._scalar_relationship(),
        )
        return self.compute_objective(profile)

    def compute_gradients(self, profile: EquilibriumProfile) -> tuple[torch.Tensor, torch.Tensor]:
        raise NotImplementedError("Gradient TBD for Tullock contest equilibrium.")

    def _support_indices(self, x: torch.Tensor, threshold: float = 1e-8) -> torch.Tensor:
        mask = x > threshold
        return mask.nonzero(as_tuple=False).squeeze(-1)

    def _record_support_metadata(self, support: torch.Tensor) -> None:
        if support.numel() == 0:
            gcd_value = torch.zeros((), dtype=torch.int64)
        else:
            # Use builtin gcd utilities via torch to summarize support spacing.
            gcd_value = tensor_gcd(support + 1)  # shift to 1-based indexing before gcd
        metadata = {
            "support_size": torch.tensor(float(support.numel())),
            "support_gcd": gcd_value.to(dtype=torch.float32),
        }
        self.record_metrics(metadata)

    def _scalar_relationship(self) -> float:
        factors = self.game.scaling_factors()
        ratio = factors[1] / torch.clamp(factors[0], min=self.game.epsilon)
        return ratio.item()

    def compute_gradients(self, profile: EquilibriumProfile) -> tuple[torch.Tensor, torch.Tensor]:
        raise NotImplementedError("Gradient TBD for Tullock contest equilibrium.")
