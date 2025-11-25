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

    loss_total: float
    loss_cls: float
    loss_reg: float
    classification: ClassificationMetrics
    regression: RegressionMetrics
