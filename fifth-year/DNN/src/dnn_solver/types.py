from typing import Optional, Tuple, List
from pydantic import BaseModel
from dataclasses import dataclass
from typing import Dict
from torch import Tensor
import torch


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
    brightness: float = 0.1  # factor sampled from [1 - b, 1 + b]
    contrast: float = 0.1    # factor sampled from [1 - c, 1 + c]
    probability: float = 0.3


class RandomDeleteAugmentationConfig(BaseModel):
    probability: float = 0.3
    scale: Tuple[float, float] = (0.02, 0.1)
    ratio: Tuple[float, float] = (0.3, 3.3)


class DataAugmentationConfig(BaseModel):
    flip: Optional[FlipAugmentationConfig] = FlipAugmentationConfig()
    rotation: Optional[RotationAugmentationConfig] = RotationAugmentationConfig()
    gaussian_noise: Optional[GaussianNoiseAugmentationConfig] = GaussianNoiseAugmentationConfig()
    brightness_contrast: Optional[BrightnessContrastAugmentationConfig] = BrightnessContrastAugmentationConfig()
    random_delete: Optional[RandomDeleteAugmentationConfig] = RandomDeleteAugmentationConfig()

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


@dataclass
class ClassificationMetrics:

    # Final metrics
    top1_acc: float = 0.0
    macro_f1: float = 0.0
    per_pair_acc: Dict[str, float] = None

    # Internal accumulators 
    _pair_names: List[str] = None
    _num_values: int = 0
    _values_min: int = 1
    _values_max: int = 9
    _correct_top1: int = 0
    _total_samples: int = 0
    _pair_correct: List[int] = None
    _pair_count: List[int] = None
    _all_true: List[Tensor] = None
    _all_pred: List[Tensor] = None

    @classmethod
    def accumulator(
        cls,
        pair_names: List[str],
        num_values: int,
        values_min: int,
        values_max: int,
    ) -> "ClassificationMetrics":
        return cls(
            per_pair_acc={},
            _pair_names=pair_names,
            _num_values=num_values,
            _values_min=values_min,
            _values_max=values_max,
            _correct_top1=0,
            _total_samples=0,
            _pair_correct=[0 for _ in pair_names],
            _pair_count=[0 for _ in pair_names],
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

        batch_size = count_targets.size(0)
        self._total_samples += batch_size

        config_ids = counts_to_config_ids(
            count_targets,
            values_min=self._values_min,
            values_max=self._values_max,
        )  # [B]
        preds = log_probs.argmax(dim=1)  # [B]

        self._correct_top1 += int((preds == config_ids).sum().item())
        self._all_true.append(config_ids.detach().cpu())
        self._all_pred.append(preds.detach().cpu())

        pair_true = (config_ids // self._num_values).detach().cpu()  # [B]
        pair_pred = (preds // self._num_values).detach().cpu()  # [B]
        for k in range(len(self._pair_names)):
            mask_k = pair_true == k
            n_k = int(mask_k.sum().item())
            if n_k == 0:
                continue
            self._pair_count[k] += n_k
            self._pair_correct[k] += int((pair_pred[mask_k] == k).sum().item())

    def aggregate(self) -> "ClassificationMetrics":
        from sklearn.metrics import f1_score

        if self._total_samples == 0:
            raise RuntimeError("No samples accumulated for classification metrics.")

        acc_top1 = float(self._correct_top1 / self._total_samples)
        y_true = torch.cat(self._all_true).numpy()
        y_pred = torch.cat(self._all_pred).numpy()
        macro_f1 = float(f1_score(y_true, y_pred, average="macro", zero_division=0))

        per_pair_acc: Dict[str, float] = {}
        for k, pname in enumerate(self._pair_names):
            if self._pair_count[k] > 0:
                per_pair_acc[pname] = float(self._pair_correct[k] / self._pair_count[k])
            else:
                per_pair_acc[pname] = float("nan")

        return ClassificationMetrics(
            top1_acc=acc_top1,
            macro_f1=macro_f1,
            per_pair_acc=per_pair_acc,
        )

    def __str__(self) -> str:
        pair_acc_items = [
            (p, a) for p, a in (self.per_pair_acc or {}).items() if not (a != a)
        ]
        pair_acc_items.sort(key=lambda x: x[1])
        worst_pairs = {p: round(a, 4) for p, a in pair_acc_items[:5]}
        return (
            f"top1={self.top1_acc:.4f}, macro_f1={self.macro_f1:.4f}, "
            f"worst_pairs={worst_pairs}"
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
    _sum_sq_err: Tensor = None
    _sum_abs_err: Tensor = None
    _total_samples: int = 0
    _target_names: List[str] = None

    @classmethod
    def accumulator(cls, target_names: List[str], device: torch.device) -> "RegressionMetrics":
        num_dims = len(target_names)
        return cls(
            rmse_per_dim={},
            mae_per_dim={},
            _sum_sq_err=torch.zeros(num_dims, device=device),
            _sum_abs_err=torch.zeros(num_dims, device=device),
            _total_samples=0,
            _target_names=target_names,
        )

    def add_batch(self, counts_pred: Tensor, count_targets: Tensor) -> None:
        diff = counts_pred - count_targets  # [B, 6]
        self._sum_sq_err += (diff ** 2).sum(dim=0)
        self._sum_abs_err += diff.abs().sum(dim=0)
        self._total_samples += counts_pred.size(0)

    def aggregate(self) -> "RegressionMetrics":
        if self._total_samples == 0:
            raise RuntimeError("No samples accumulated for regression metrics.")
        rmse_per_dim = torch.sqrt(self._sum_sq_err / self._total_samples)
        mae_per_dim = self._sum_abs_err / self._total_samples

        rmse_overall = float(torch.sqrt((self._sum_sq_err / self._total_samples).mean()).item())
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
    """Top-level container returned by `MultiTaskTrainer.evaluate`."""

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
            f"[Loss] {self.loss}; "
            f"[Classification] {self.classification}; "
            f"[Regression] {self.regression}"
        )

    __repr__ = __str__
