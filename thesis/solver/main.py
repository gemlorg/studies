from __future__ import annotations

import argparse

import torch

from solver.algorithms.tullock import TullockGradientDescentSolver
from solver.config import SolverConfig
from solver.results import EquilibriumProfile
from examples.identity_tullock import build_identity_tullock_game


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run Tullock equilibrium solver prototype.")
    parser.add_argument("--dimension", type=int, default=5, help="Number of fields (size of identity matrix).")
    parser.add_argument("--lambda-param", type=float, default=0.5, help="Tullock contest exponent λ ∈ [0, 1].")
    parser.add_argument("--seed", type=int, default=42, help="Random seed for candidate sampling.")
    return parser.parse_args()


def summarize_profile(profile: EquilibriumProfile) -> str:
    support_one = (profile.player_one > 1e-8).sum().item()
    support_two = (profile.player_two > 1e-8).sum().item()
    return (
        f"player_one_norm={profile.player_one.norm().item():.4f} "
        f"player_two_norm={profile.player_two.norm().item():.4f} "
        f"support_sizes=({support_one}, {support_two}) "
        f"scalar_relationship={profile.scalar_relationship:.4f}"
    )


def main() -> None:
    args = parse_args()
    game = build_identity_tullock_game(args.dimension, lambda_param=args.lambda_param)
    config = SolverConfig(random_seed=args.seed, max_iterations=5_000, tolerance=1e-7)
    solver = TullockGradientDescentSolver(game, config=config)

    generator = torch.Generator()
    generator.manual_seed(args.seed)
    candidate_x = torch.rand(game.field_count, generator=generator)

    strategies = game.strategies_from_x(candidate_x)
    scaling_factors = game.scaling_factors()
    scalar_relationship = (scaling_factors[1] / torch.clamp(scaling_factors[0], min=game.epsilon)).item()
    profile = EquilibriumProfile(player_one=strategies[0], player_two=strategies[1], scalar_relationship=scalar_relationship)
    loss_value = solver.loss_from_x(candidate_x)
    payoffs = game.expected_payoffs(game.strategies_from_x(candidate_x))
    violation = game.constraint_violation(candidate_x)

    print("=== Tullock Identity Scenario ===")
    print(f"dimension={args.dimension} lambda={args.lambda_param}")
    print("profile:", summarize_profile(profile))
    print(f"loss={loss_value.item():.6f}")
    print(f"payoffs=({payoffs[0].item():.6f}, {payoffs[1].item():.6f})")
    print(f"max_constraint_violation={violation.max().item():.6f}")
    print("Note: solver.solve() still requires gradient implementation.")


if __name__ == "__main__":
    main()
