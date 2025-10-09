# Equilibrium Solver (Design Draft)

This project lays out the structure for an equilibrium solver for two-player investment games. It uses [uv](https://github.com/astral-sh/uv) for dependency management and [PyTorch](https://pytorch.org) for tensor operations.

## Project Setup

```bash
uv sync
uv run python -m solver.examples.draft
uv run python main.py --dimension 5 --lambda-param 0.5
```

The solver package lives in `src/solver`. Test files will go under `tests/` when they are added.

## Development Status

Current focus is on Tullock contest success functions (λ∈[0,1]). The framework includes:
- `TullockContestGame` for calibrated probability and constraint evaluation
- `TullockGradientDescentSolver` that maps shared support vectors `x` to player strategies via the closed-form scaling relationship
- Utility hooks for support analysis (including a tensor GCD helper) and loss construction based on constraint violations and payoffs

The actual optimization routines and gradient derivations remain TODO until the final equilibrium objective is locked down.

## Identity Scenario

The `examples/identity_tullock.py` module defines a canonical setup where:
- `A = I_n`
- Field values form the sequence `1..n`
- Player costs are fixed at `(1, 2)`

`main.py` wires this scenario into the solver and reports the provisional loss and constraint diagnostics for a sampled support vector `x`.

## License

This repository currently has no license specified.
