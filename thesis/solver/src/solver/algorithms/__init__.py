"""Algorithm registry for the equilibrium solver framework."""

from .gradient_descent import GradientDescentHyperParams, GradientDescentSolver
from .tullock import TullockGradientDescentSolver

__all__ = ["GradientDescentHyperParams", "GradientDescentSolver", "TullockGradientDescentSolver"]
