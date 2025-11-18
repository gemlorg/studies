from typing import Optional, Tuple, List
from pydantic import BaseModel
from dataclasses import dataclass, field
from typing import Dict
from sklearn.metrics import confusion_matrix
from torch import Tensor
import torch

# Configurations for data augmentation
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

class BrightnessContrastAugmentationConfig(BaseModel):
    brightness: float = 0.1
    contrast: float = 0.1
    probability: float = 0.3

class DataAugmentationConfig(BaseModel):
    flip: Optional[FlipAugmentationConfig] = FlipAugmentationConfig()
    rotation: Optional[RotationAugmentationConfig] = RotationAugmentationConfig()
    gaussian_noise: Optional[GaussianNoiseAugmentationConfig] = GaussianNoiseAugmentationConfig()
    brightness_contrast: Optional[BrightnessContrastAugmentationConfig] = BrightnessContrastAugmentationConfig()


@dataclass
class MultiTaskLossOutput:
    total: Tensor
    cls: Tensor
    reg: Tensor

    def __add__(self, other: object) -> "MultiTaskLossOutput":
        if not isinstance(other, MultiTaskLossOutput):
            return NotImplemented
        return MultiTaskLossOutput(
            total=self.total + other.total,
            cls=self.cls + other.cls,
            reg=self.reg + other.reg,
        )

    def __radd__(self, other: object) -> "MultiTaskLossOutput":
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
    def detach(self) -> "MultiTaskLossOutput":
        """Return a new loss container with detached tensors."""
        return MultiTaskLossOutput(
            total=self.total.detach(),
            cls=self.cls.detach(),
            reg=self.reg.detach(),
        )

    def __str__(self) -> str:
        return (
            f"loss_total={float(self.total):.4f}, "
            f"loss_cls={float(self.cls):.4f}, "
            f"loss_reg={float(self.reg):.4f}"
        )

    __repr__ = __str__


# all metrics we want to hold
@dataclass
class ClassificationMetrics:

    # Final metrics
    top1_acc: float = 0.0
    macro_f1: float = 0.0
    per_pair_acc: Dict[str, float] = None
    # Pair-level confusion matrix, row-normalized (shape [num_pairs, num_pairs]).
    confusion_matrix: Optional[Tensor] = None
    # Names of unordered shape pairs, aligned with confusion_matrix axes.
    pair_names: Optional[List[str]] = None

    # Internal accumulators 
    _pair_names: List[str] = field(default_factory=list)
    _values_min: int = 1
    _values_max: int = 9
    _all_true: List[Tensor] = field(default_factory=list)
    _all_pred: List[Tensor] = field(default_factory=list)

    @classmethod
    def accumulator(
        cls,
        pair_names: List[str],
        values_min: int,
        values_max: int,
    ) -> "ClassificationMetrics":
        return cls(
            per_pair_acc={},
            _pair_names=pair_names,
            _values_min=values_min,
            _values_max=values_max,
            _all_true=[],
            _all_pred=[],
        )

    def add_batch(
        self,
        log_probs: Tensor,
        counts_pred: Tensor,
        count_targets: Tensor,
    ) -> None:
        from dnn_solver.utils import counts_to_config_ids

        config_ids = counts_to_config_ids(
            count_targets,
            values_min=self._values_min,
            values_max=self._values_max,
        )  # [B]
        preds = log_probs.argmax(dim=1)  # [B]

        self._all_true.append(config_ids.detach().cpu())
        self._all_pred.append(preds.detach().cpu())

    def aggregate(self) -> "ClassificationMetrics":
        from sklearn.metrics import f1_score

        if not self._all_true:
            raise RuntimeError("No samples accumulated for classification metrics.")

        num_values = self._values_max - self._values_min + 1
        y_true_t = torch.cat(self._all_true)
        y_pred_t = torch.cat(self._all_pred)

        acc_top1 = float((y_true_t == y_pred_t).float().mean().item())
        y_true = y_true_t.numpy()
        y_pred = y_pred_t.numpy()
        macro_f1 = float(f1_score(y_true, y_pred, average="macro", zero_division=0))

        per_pair_acc: Dict[str, float] = {}
        # class id // num_values = pair index
        pair_true = (y_true_t // num_values)
        pair_pred = (y_pred_t // num_values)

        for k, pname in enumerate(self._pair_names):
            mask = pair_true == k
            count_k = int(mask.sum().item())
            if count_k == 0:
                per_pair_acc[pname] = float("nan")
            else:
                per_pair_acc[pname] = float((pair_pred[mask] == k).float().mean().item())

        # visualize which pairse are confused with which
        num_pairs = len(self._pair_names)
        cm = confusion_matrix(
            pair_true.numpy(),
            pair_pred.numpy(),
            labels=list(range(num_pairs)),
            normalize="true",
        )

        return ClassificationMetrics(
            top1_acc=acc_top1,
            macro_f1=macro_f1,
            per_pair_acc=per_pair_acc,
            confusion_matrix=torch.tensor(cm, dtype=torch.float32),
            pair_names=self._pair_names,
        )

    def __str__(self) -> str:
        # show 5 worst accuracy pairs
        pair_acc_items = [
            (p, a) for p, a in (self.per_pair_acc or {}).items() if not (a != a)
        ]
        pair_acc_items.sort(key=lambda x: x[1])
        worst_pairs = {p: round(a, 4) for p, a in pair_acc_items[:5]}
        return (
            f"top1={self.top1_acc:.4f}, macro_f1={self.macro_f1:.4f}, "
            f"lowest_acc_pairs={worst_pairs}"
        )

    __repr__ = __str__


@dataclass
class RegressionMetrics:
    """6-D count regression metrics."""

    rmse_overall: float = 0.0
    mae_overall: float = 0.0
    rmse_per_dim: Dict[str, float] = None
    mae_per_dim: Dict[str, float] = None

    # Internal accumulators
    _target_names: List[str] = None
    _preds: List[Tensor] = None
    _targets: List[Tensor] = None

    @classmethod
    def accumulator(cls, target_names: List[str], device: torch.device) -> "RegressionMetrics":
        return cls(
            rmse_per_dim={},
            mae_per_dim={},
            _target_names=target_names,
            _preds=[],
            _targets=[],
        )

    def add_batch(self, counts_pred: Tensor, count_targets: Tensor) -> None:
        self._preds.append(counts_pred.detach().cpu())
        self._targets.append(count_targets.detach().cpu())

    def aggregate(self) -> "RegressionMetrics":
        if not self._preds:
            raise RuntimeError("No samples accumulated for regression metrics.")

        preds = torch.cat(self._preds)
        targets = torch.cat(self._targets)
        diff = preds - targets
        rmse_per_dim = torch.sqrt((diff ** 2).mean(dim=0))
        mae_per_dim = diff.abs().mean(dim=0)

        rmse_overall = float(rmse_per_dim.mean().item())
        mae_overall = float(mae_per_dim.mean().item())

        rmse_per_dim_dict = {
            name: float(val) for name, val in zip(self._target_names, rmse_per_dim.detach().cpu())
        }
        mae_per_dim_dict = {
            name: float(val) for name, val in zip(self._target_names, mae_per_dim.detach().cpu())
        }

        return RegressionMetrics(
            rmse_overall=rmse_overall,
            mae_overall=mae_overall,
            rmse_per_dim=rmse_per_dim_dict,
            mae_per_dim=mae_per_dim_dict,
        )

    def __str__(self) -> str:
        rmse_str = {k: round(v, 4) for k, v in (self.rmse_per_dim or {}).items()}
        mae_str = {k: round(v, 4) for k, v in (self.mae_per_dim or {}).items()}
        return (
            f"rmse_overall={self.rmse_overall:.4f}, mae_overall={self.mae_overall:.4f}, "
            f"rmse_per_dim={rmse_str}, mae_per_dim={mae_str}"
        )

    __repr__ = __str__


@dataclass
class MultiTaskMetrics:
    loss: MultiTaskLossOutput
    classification: ClassificationMetrics
    regression: RegressionMetrics

    @property
    def loss_total(self) -> float:
        return float(self.loss.total)

    @property
    def loss_cls(self) -> float:
        return float(self.loss.cls)

    @property
    def loss_reg(self) -> float:
        return float(self.loss.reg)

    def __str__(self) -> str:
        return (
            f"[Loss] {self.loss}; \n"
            f"[Classification] {self.classification}; \n"
            f"[Regression] {self.regression}"
        )

    __repr__ = __str__
