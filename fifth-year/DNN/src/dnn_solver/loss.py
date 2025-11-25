from dnn_solver.types import MultiTaskLossOutput
from dnn_solver.utils import counts_to_config_ids
import torch.nn as nn
import torch.nn.functional as F
from torch import Tensor

# Allow custom (non-ipywidget) widgets.


from typing import Literal


class GSNMultiTaskLoss(nn.Module):
    """
    Multitask loss for Graph ShapeNet-style counting.

    - Classification head predicts a configuration over shape/count patterns.
    - Regression head predicts the 6 raw counts.
    - The ground-truth labels are ONLY the 6D counts; configuration IDs are
      computed internally from those counts.

    Args
    ----
    lambda_cnt:
        Weight for the regression (count) loss term.
    reduction:
        Reduction applied to individual NLL and SmoothL1 losses.
        One of {"mean", "sum", "none"}.
    values_min:
        Minimum non-zero count value used in the configuration encoding.
    values_max:
        Maximum non-zero count value used in the configuration encoding.
    """

    def __init__(
        self,
        lambda_cnt: float = 1.0,
        use_cls_loss: bool = True,
        reduction: Literal["mean", "sum", "none"] = "mean",
        values_min: int = 2,
        values_max: int = 8,
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
        cls_logits: Tensor,  # [B, 1]
        counts_pred: Tensor,  # [B, 6]
        count_targets: Tensor,  # [B, 6]
    ) -> MultiTaskLossOutput:
        """
        Compute multitask loss.

        Args:
            cls_logits:
                Raw (un-normalized) scores from classification head, shape [B, C].
            counts_pred:
                Predicted counts from regression head, shape [B, 6].
            count_targets:
                Ground-truth counts, shape [B, 6].

        Returns:
            MultiTaskLossOutput with total / cls / reg components.
            You typically do: `loss = criterion(...).total`
        """
        if count_targets.ndim != 2 or count_targets.size(1) != 6:
            raise ValueError(
                f"Expected count_targets of shape [B, 6], got {tuple(count_targets.shape)}"
            )
        if cls_logits.ndim != 2 or cls_logits.size(1) != 105:
            raise ValueError(
                f"Expected cls_logits of shape [B, 105], got {tuple(cls_logits.shape)}"
            )
        if counts_pred.ndim != 2 or counts_pred.size(1) != 6:
            raise ValueError(
                f"Expected counts_pred of shape [B, 6], got {tuple(counts_pred.shape)}"
            )

        # 1) Compute configuration IDs from counts.
        config_ids = counts_to_config_ids(
            count_targets,
            values_min=self.values_min,
            values_max=self.values_max,
        )  # [B]

        # 2) Classification loss: NLL over log-softmaxed logits.
        log_probs = F.log_softmax(cls_logits, dim=1)  # [B, num_classes]
        loss_cls = self._cls_loss(log_probs, config_ids)

        # 3) Regression loss over the raw counts.
        loss_reg = self._reg_loss(counts_pred, count_targets)

        # 4) Total multitask loss.
        loss_total = loss_cls  + self.lambda_cnt * loss_reg

        return MultiTaskLossOutput(total=loss_total, cls=loss_cls, reg=loss_reg)
