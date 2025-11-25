from dnn_solver.types import MultiTaskLossOutput
from dnn_solver.utils import counts_to_config_ids
import torch.nn as nn
from torch import Tensor
from typing import Literal


class GSNMultiTaskLoss(nn.Module):

    def __init__(
        self,
        lambda_cnt: float = 1.0,
        use_cls_loss: bool = True,
        reduction: Literal["mean", "sum", "none"] = "mean",
        values_min: int = 1,
        values_max: int = 9,
    ) -> None:
        super().__init__()
        self.lambda_cnt = float(lambda_cnt)
        self.reduction = reduction
        self.use_cls_loss = use_cls_loss
        self.values_min = int(values_min)
        self.values_max = int(values_max)

        self._cls_loss = nn.NLLLoss(reduction=reduction)
        self._reg_loss = nn.SmoothL1Loss(reduction=reduction)

    def forward(
        self,
        log_probs: Tensor,  # [B, num_classes], already log-softmaxed
        counts_pred: Tensor,  # [B, 6]
        count_targets: Tensor,  # [B, 6]
    ) -> MultiTaskLossOutput:
        if count_targets.ndim != 2 or count_targets.size(1) != 6:
            raise ValueError(
                f"Expected count_targets of shape [B, 6], got {tuple(count_targets.shape)}"
            )
        if log_probs.ndim != 2:
            raise ValueError(
                f"Expected log_probs of shape [B, C], got {tuple(log_probs.shape)}"
            )
        if counts_pred.ndim != 2 or counts_pred.size(1) != 6:
            raise ValueError(
                f"Expected counts_pred of shape [B, 6], got {tuple(counts_pred.shape)}"
            )

        # Compute how many config classes we expect for the chosen value range.
        num_dims = count_targets.size(1)
        num_pairs = num_dims * (num_dims - 1) // 2
        expected_num_classes = num_pairs * (self.values_max - self.values_min + 1)
        if log_probs.size(1) != expected_num_classes:
            raise ValueError(
                f"Expected log_probs second dim {expected_num_classes} for value range "
                f"[{self.values_min},{self.values_max}], got {log_probs.size(1)}"
            )

        # 1) Compute configuration IDs from counts.
        config_ids = counts_to_config_ids(
            count_targets,
            values_min=self.values_min,
            values_max=self.values_max,
        )  # [B]

        # 2) Classification loss: NLL over provided log-probs.
        if self.use_cls_loss:
            loss_cls = self._cls_loss(log_probs, config_ids)
        else:
            loss_cls = counts_pred.new_zeros(())

        # 3) Regression loss over the raw counts.
        loss_reg = self._reg_loss(counts_pred, count_targets)

        # 4) Total multitask loss.
        loss_total = loss_cls * int(self.use_cls_loss) + self.lambda_cnt * loss_reg

        return MultiTaskLossOutput(total=loss_total, cls=loss_cls, reg=loss_reg)
