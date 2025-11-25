from typing import Optional, Tuple
from pydantic import BaseModel
from dataclasses import dataclass
from typing import Dict
from torch import Tensor


class FlipAugmentationConfig(BaseModel):
    horizontal: bool = True
    vertical: bool = True
    probability: float = 0.3


class RotationAugmentationConfig(BaseModel):
    probability: float = 0.3


class GaussianNoiseAugmentationConfig(BaseModel):
    mean: float = 0.0
    stddev: float = 0.1
    probability: float = 0.3


class RandomDeleteAugmentationConfig(BaseModel):
    probability: float = 0.3
    scale: Tuple[float, float] = (0.02, 0.1)
    ratio: Tuple[float, float] = (0.3, 3.3)


class DataAugmentationConfig(BaseModel):
    flip: Optional[FlipAugmentationConfig] = FlipAugmentationConfig()
    rotation: Optional[RotationAugmentationConfig] = RotationAugmentationConfig()
    gaussian_noise: Optional[GaussianNoiseAugmentationConfig] = GaussianNoiseAugmentationConfig()
    random_delete: Optional[RandomDeleteAugmentationConfig] = RandomDeleteAugmentationConfig()

@dataclass
class MultiTaskLossOutput:
    """Convenient container for logging the components of the loss."""

    total: Tensor
    cls: Tensor
    reg: Tensor

    def __add__(self, other: object) -> "MultiTaskLossOutput":
        """
        Support elementwise addition so callers can accumulate losses without
        manually constructing dicts. Non-MultiTaskLossOutput operands return
        NotImplemented to allow Python to fall back to other __radd__.
        """
        if not isinstance(other, MultiTaskLossOutput):
            return NotImplemented
        return MultiTaskLossOutput(
            total=self.total + other.total,
            cls=self.cls + other.cls,
            reg=self.reg + other.reg,
        )

    def __radd__(self, other: object) -> "MultiTaskLossOutput":
        """
        Allow use with built-ins like sum(); treat 0 as the neutral element.
        """
        if other == 0:
            return self
        return self.__add__(other)
    def scale(self, factor: float) -> "MultiTaskLossOutput":
        """Scale all loss components by the given factor."""
        return MultiTaskLossOutput(
            total=self.total * factor,
            cls=self.cls * factor,
            reg=self.reg * factor,
        )


@dataclass
class ClassificationMetrics:
    """135-way classification metrics."""

    top1_acc: float  # Top-1 accuracy over all configs
    macro_f1: float  # Macro F1 over 135 classes
    per_pair_acc: Dict[str, float]  # Accuracy aggregated by unordered shape pair


@dataclass
class RegressionMetrics:
    """6-D count regression metrics."""

    rmse_overall: float  # RMSE averaged over all 6 dims
    mae_overall: float  # MAE averaged over all 6 dims
    rmse_per_dim: Dict[str, float]  # RMSE per shape (squares, circles, ...)
    mae_per_dim: Dict[str, float]  # MAE per shape (squares, circles, ...)


@dataclass
class MultiTaskMetrics:
    """Top-level container returned by `MultiTaskTrainer.evaluate`."""

    loss: MultiTaskLossOutput
    classification: ClassificationMetrics
    regression: RegressionMetrics
