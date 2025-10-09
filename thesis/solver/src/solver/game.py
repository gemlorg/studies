from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Protocol, Tuple

import torch


class ProbabilityFunction(Protocol):
    """Callable that maps per-field scores to win probabilities for player one."""

    def __call__(self, scores_x: torch.Tensor, scores_y: torch.Tensor) -> torch.Tensor: ...


@dataclass(frozen=True)
class InvestmentProfile:
    """Represents a single player's strategy and its induced investments."""

    strategy: torch.Tensor
    investments: torch.Tensor


@dataclass(frozen=True)
class GameSpec:
    """Immutable description of the two-player investment game."""

    matrix: torch.Tensor
    probability_fn: ProbabilityFunction
    investment_costs: torch.Tensor
    field_values: torch.Tensor
    device: torch.device | None = None
    dtype: torch.dtype | None = None
    _validated: bool = field(default=False, init=False, repr=False, compare=False)

    def to_device(self, device: torch.device) -> "GameSpec":
        """Return a copy of the game spec moved to the requested device."""
        return GameSpec(
            matrix=self.matrix.to(device),
            probability_fn=self.probability_fn,
            investment_costs=self.investment_costs.to(device),
            field_values=self.field_values.to(device),
            device=device,
            dtype=self.dtype,
        )

    def with_dtype(self, dtype: torch.dtype) -> "GameSpec":
        """Return a copy of the game spec cast to the requested dtype."""
        return GameSpec(
            matrix=self.matrix.to(dtype=dtype),
            probability_fn=self.probability_fn,
            investment_costs=self.investment_costs.to(dtype=dtype),
            field_values=self.field_values.to(dtype=dtype),
            device=self.device,
            dtype=dtype,
        )

    def ensure_valid(self) -> "GameSpec":
        """Run structural checks once to guard downstream algorithms."""
        if self._validated:
            return self

        if self.matrix.ndim != 2:
            raise ValueError("A must be a 2D matrix")

        n_players = 2
        if self.investment_costs.shape != (n_players,):
            raise ValueError("investment_costs must have shape (2,)")

        if self.matrix.shape[0] != self.field_values.shape[0]:
            raise ValueError("Number of rows in A must match length of field_values")

        if self.matrix.shape[0] == 0:
            raise ValueError("A must have at least one field")

        if self.matrix.shape[1] == 0:
            raise ValueError("A must have at least one decision variable per player")

        # Basic sanity check for dtype/device alignment.
        if self.device and self.matrix.device != self.device:
            raise ValueError("Matrix device disagrees with declared device")

        if self.dtype and self.matrix.dtype != self.dtype:
            raise ValueError("Matrix dtype disagrees with declared dtype")

        object.__setattr__(self, "_validated", True)
        return self

    @property
    def decision_dim(self) -> int:
        """Number of decision variables available to each player."""
        return self.matrix.shape[1]

    @property
    def field_count(self) -> int:
        """Number of fields (rows) scored by the matrix A."""
        return self.matrix.shape[0]


class TullockContestGame(GameSpec):
    """Specialization of the game spec where the probability follows a Tullock CSF."""

    lambda_param: float
    epsilon: float

    def __init__(
        self,
        *,
        matrix: torch.Tensor,
        investment_costs: torch.Tensor,
        field_values: torch.Tensor,
        lambda_param: float,
        epsilon: float = 1e-12,
        device: torch.device | None = None,
        dtype: torch.dtype | None = None,
    ) -> None:
        lambda_param = float(lambda_param)
        if not 0.0 <= lambda_param <= 1.0:
            raise ValueError("lambda must be within [0, 1] for a Tullock contest.")

        epsilon = float(epsilon)
        if epsilon <= 0.0:
            raise ValueError("epsilon must be strictly positive.")

        probability_fn = self._build_probability_fn(lambda_param, epsilon)
        super().__init__(
            matrix=matrix,
            probability_fn=probability_fn,
            investment_costs=investment_costs,
            field_values=field_values,
            device=device,
            dtype=dtype,
        )
        object.__setattr__(self, "lambda_param", lambda_param)
        object.__setattr__(self, "epsilon", epsilon)

    @staticmethod
    def _build_probability_fn(lambda_param: float, epsilon: float) -> ProbabilityFunction:
        def tullock_probability(scores_x: torch.Tensor, scores_y: torch.Tensor) -> torch.Tensor:
            scores_x = torch.clamp(scores_x, min=0.0)
            scores_y = torch.clamp(scores_y, min=0.0)

            if lambda_param == 0.0:
                base_value = torch.full_like(scores_x, 0.5)
                return base_value

            power_x = torch.pow(scores_x, lambda_param)
            power_y = torch.pow(scores_y, lambda_param)
            denominator = power_x + power_y
            numerator = power_x

            base_value = torch.full_like(denominator, 0.5)
            safe_denominator = torch.clamp(denominator, min=epsilon)
            ratio = numerator / safe_denominator
            return torch.where(denominator > epsilon, ratio, base_value)

        return tullock_probability

    def to_device(self, device: torch.device) -> "TullockContestGame":
        return TullockContestGame(
            matrix=self.matrix.to(device),
            investment_costs=self.investment_costs.to(device),
            field_values=self.field_values.to(device),
            lambda_param=self.lambda_param,
            epsilon=self.epsilon,
            device=device,
            dtype=self.dtype,
        )

    def with_dtype(self, dtype: torch.dtype) -> "TullockContestGame":
        return TullockContestGame(
            matrix=self.matrix.to(dtype=dtype),
            investment_costs=self.investment_costs.to(dtype=dtype),
            field_values=self.field_values.to(dtype=dtype),
            lambda_param=self.lambda_param,
            epsilon=self.epsilon,
            device=self.device,
            dtype=dtype,
        )

    def scaling_factors(self) -> torch.Tensor:
        """Return the per-player scalar multiplying the shared ``x`` vector."""
        costs = self.investment_costs.to(dtype=self.matrix.dtype, device=self.matrix.device)
        c1, c2 = costs
        lam = self.lambda_param
        product_term = (c1 * c2) ** lam
        denominator = (c1**lam + c2**lam) ** 2
        base = lam * product_term / torch.clamp(denominator, min=self.epsilon)
        scale_player_one = base / torch.clamp(c1, min=self.epsilon)
        scale_player_two = base / torch.clamp(c2, min=self.epsilon)
        return torch.tensor([scale_player_one, scale_player_two], dtype=self.matrix.dtype, device=self.matrix.device)

    def strategies_from_x(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """Construct the pair of player strategies implied by a candidate ``x``."""
        factors = self.scaling_factors().to(dtype=x.dtype, device=x.device)
        strategy_one = factors[0] * x
        strategy_two = factors[1] * x
        return strategy_one, strategy_two

    def recover_x(self, strategies: Tuple[torch.Tensor, torch.Tensor]) -> torch.Tensor:
        """Estimate the common ``x`` vector from both players' strategies."""
        factors = self.scaling_factors().to(dtype=strategies[0].dtype, device=strategies[0].device)
        estimates = []
        for idx, strategy in enumerate(strategies):
            scale = torch.clamp(factors[idx], min=self.epsilon)
            estimates.append(strategy / scale)
        stacked = torch.stack(estimates, dim=0)
        return torch.mean(stacked, dim=0)

    def constraint_violation(self, x: torch.Tensor) -> torch.Tensor:
        """Evaluate the inequality constraint A diag(v) 1 / (A^T x) <= 1."""
        ones = torch.ones(self.field_values.shape[0], dtype=x.dtype, device=x.device)
        weighted_fields = self.field_values.to(dtype=x.dtype, device=x.device) * ones
        numerator = torch.matmul(self.matrix.to(dtype=x.dtype, device=x.device), weighted_fields)
        denom = torch.matmul(self.matrix.to(dtype=x.dtype, device=x.device).T, x)
        safe_denom = torch.clamp(denom, min=self.epsilon)
        ratio = numerator / safe_denom
        return ratio - 1.0

    def expected_payoffs(self, strategies: Tuple[torch.Tensor, torch.Tensor]) -> Tuple[torch.Tensor, torch.Tensor]:
        """Compute the expected payoffs given player strategies."""
        strategy_one, strategy_two = strategies
        investments_one = torch.matmul(self.matrix, strategy_one)
        investments_two = torch.matmul(self.matrix, strategy_two)
        win_probs = self.probability_fn(investments_one, investments_two)
        field_values = self.field_values.to(dtype=win_probs.dtype, device=win_probs.device)
        payoff_one = torch.sum(win_probs * field_values)
        payoff_two = torch.sum((1 - win_probs) * field_values)
        return payoff_one, payoff_two
