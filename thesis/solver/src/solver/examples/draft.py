from __future__ import annotations

import torch

from ..config import SolverConfig
from ..game import TullockContestGame
from ..algorithms.tullock import TullockGradientDescentSolver


def build_mock_game() -> TullockContestGame:
    matrix = torch.eye(3)
    investment_costs = torch.tensor([1.0, 1.0])
    field_values = torch.tensor([1.0, 0.8, 1.2])
    return TullockContestGame(
        matrix=matrix,
        investment_costs=investment_costs,
        field_values=field_values,
        lambda_param=0.5,
    )


def main() -> None:
    game = build_mock_game()
    config = SolverConfig(max_iterations=1_000, tolerance=1e-6)
    solver = TullockGradientDescentSolver(game, config=config)
    print("Solver design ready:", solver)
    candidate_x = torch.rand(game.field_count)
    loss = solver.loss_from_x(candidate_x)
    print("Sample loss from random x:", loss.item())
    print("Call solver.solve() once Tullock objective/gradients are implemented.")


if __name__ == "__main__":
    main()
