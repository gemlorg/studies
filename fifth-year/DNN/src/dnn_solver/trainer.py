from copy import deepcopy
from dnn_solver.constants import TARGET_COL_NAMES
from dnn_solver.loss import GSNMultiTaskLoss
from dnn_solver.types import ClassificationMetrics, MultiTaskLossOutput, MultiTaskMetrics, RegressionMetrics
from dnn_solver.utils import counts_to_config_ids
import torch
import torch.nn as nn
from torch.utils.data import DataLoader
from tqdm import tqdm

# Allow custom (non-ipywidget) widgets.


from typing import Dict, List, Optional



class MultiTaskTrainer:

    def __init__(
        self,
        model: nn.Module,
        trainloader: DataLoader,
        testloader: DataLoader,
        device: torch.device,
        lambda_cnt: float,
        use_cls_loss: bool = True,
        values_min: int = 2,
        values_max: int = 8,
    ) -> None:
        self.device = torch.device(device)
        self.model = model.to(self.device)
        self.trainloader = trainloader

        self.testloader = testloader

        self.criterion = GSNMultiTaskLoss(
            lambda_cnt=lambda_cnt,
            use_cls_loss=use_cls_loss,
            reduction="mean",
            values_min=values_min,
            values_max=values_max,
        )
        self.lambda_cnt = float(lambda_cnt)
        self.use_cls_loss = use_cls_loss
        self.values_min = values_min
        self.values_max = values_max
        self.best_model: Optional[nn.Module] = None 
        self.best_eval: Optional[MultiTaskMetrics] = None
        self.loss_history: List[MultiTaskLossOutput] = []
        self.eval_history: List[MultiTaskMetrics] = []

    def train(
        self,
        epochs: int = 100,
        lr: float = 1e-3,
        weight_decay: float = 0.0,
        log_freq: int = 50,
        patience: int = 5,
    ) -> None:
        optimizer = torch.optim.Adam(
            self.model.parameters(),
            lr=lr,
            weight_decay=weight_decay,
        )
        curr_patience = patience
        for epoch in range(epochs):
            self.model.train()
            running_loss = 0.0

            for batch_idx, (images, count_targets) in enumerate(self.trainloader):
                images = images.to(self.device)
                count_targets = count_targets.to(self.device)

                optimizer.zero_grad()
                cls_logits, counts_pred = self.model(
                    images
                )  # model must return (cls_logits, counts_pred)
                full_loss = self.criterion(cls_logits, counts_pred, count_targets)
                self.loss_history.append(full_loss)

                loss = full_loss.total
                loss.backward()
                optimizer.step()

                running_loss += loss.item()

                if (batch_idx + 1) % log_freq == 0:
                    avg_loss = running_loss / log_freq
                    print(
                        f"epoch {epoch + 1:>4}, "
                        f"batch {batch_idx + 1:>5}, "
                        f"loss {avg_loss:.3f}"
                    )
                    running_loss = 0.0

            # end of epoch: evaluate on test set
            metrics = self.evaluate()
            self.eval_history.append(metrics)
            print(f"epoch {epoch + 1:>4} evaluation: loss_total={metrics.loss_total:.4f}, accuracy={metrics.classification.top1_acc:.4f}")
            print(f"               loss_cls={metrics.loss_cls:.4f}, loss_reg={metrics.loss_reg:.4f}")

            curr_patience -= 1
            if not self.best_eval or metrics.loss_total < self.best_eval.loss_total:
                self.best_model = deepcopy(self.model)
                self.best_eval =metrics
                curr_patience = patience
            if curr_patience == 0: 
                print(f"Stopping due to early stoping at epoch {epoch+1}") 
                return

    @torch.no_grad()
    def evaluate(self) -> MultiTaskMetrics:
        """
        Evaluate on the held-out testloader and return MultiTaskMetrics.

        Classification:
            - Top-1 accuracy (135-way)
            - Macro F1-score (135-way)
            - Per-pair accuracy aggregated by unordered shape pair

        Regression (6-D counts):
            - RMSE per class (dimension) and overall
            - MAE per class (dimension) and overall
        """
        from sklearn.metrics import f1_score  # local import to keep dependencies scoped

        self.model.eval()

        num_dims = 6  # squares, circles, up, right, down, left
        num_values = self.values_max - self.values_min + 1
        num_pairs = num_dims * (num_dims - 1) // 2  # C(6, 2) = 15

        # --------- Accumulators for losses ----------
        total_samples = 0
        sum_loss_cls = 0.0
        sum_loss_reg = 0.0

        # --------- Classification metrics ----------
        correct_top1 = 0
        all_true = []  # list of 1D tensors with true config IDs
        all_pred = []  # list of 1D tensors with predicted config IDs

        correct_per_pair = torch.zeros(num_pairs, dtype=torch.long)
        count_per_pair = torch.zeros(num_pairs, dtype=torch.long)

        # --------- Regression metrics ----------
        sum_sq_err = torch.zeros(num_dims, device=self.device)  # for RMSE
        sum_abs_err = torch.zeros(num_dims, device=self.device)  # for MAE

        for images, count_targets in self.testloader:
            images = images.to(self.device)
            count_targets = count_targets.to(self.device)

            cls_logits, counts_pred = self.model(images)  # [B, C], [B, 6]
            batch_size = images.size(0)
            total_samples += batch_size

            # ----- Losses -----
            # Use the same criterion as during training.
            loss_out = self.criterion(cls_logits, counts_pred, count_targets)
            sum_loss_cls += float(loss_out.cls.item()) * batch_size
            sum_loss_reg += float(loss_out.reg.item()) * batch_size

            # ----- True configuration IDs from counts -----
            config_ids = counts_to_config_ids(
                count_targets,
                values_min=self.values_min,
                values_max=self.values_max,
            )  # [B]

            # ----- Top-1 predictions (135-way classification) -----
            preds = cls_logits.argmax(dim=1)  # [B]

            correct_top1 += int((preds == config_ids).sum().item())
            all_true.append(config_ids.detach().cpu())
            all_pred.append(preds.detach().cpu())

            # ----- Per-pair accuracy (ignore count value) -----
            # Config ID = pair_index * num_values + value_offset
            pair_true = (config_ids // num_values).detach().cpu()  # [B]
            pair_pred = (preds // num_values).detach().cpu()  # [B]

            for k in range(num_pairs):
                mask_k = pair_true == k
                n_k = int(mask_k.sum().item())
                if n_k == 0:
                    continue

                count_per_pair[k] += n_k
                correct_per_pair[k] += int((pair_pred[mask_k] == k).sum().item())

            # ----- Regression errors -----
            diff = counts_pred - count_targets  # [B, 6]
            sum_sq_err += (diff**2).sum(dim=0)
            sum_abs_err += diff.abs().sum(dim=0)

        if total_samples == 0:
            raise RuntimeError("No samples in testloader.")

        # ===============================
        #   Aggregate losses
        # ===============================
        loss_cls = sum_loss_cls / total_samples
        loss_reg = sum_loss_reg / total_samples
        loss_total = loss_cls * int(self.use_cls_loss) + self.lambda_cnt * loss_reg

        # ===============================
        #   Classification metrics
        # ===============================
        acc_top1 = correct_top1 / total_samples

        y_true = torch.cat(all_true).numpy()
        y_pred = torch.cat(all_pred).numpy()
        macro_f1 = float(f1_score(y_true, y_pred, average="macro", zero_division=0))

        # Map pair indices to human-readable unordered pairs
        pair_names = []
        idx = 0
        for i, name_i in enumerate(TARGET_COL_NAMES):
            for j in range(i + 1, len(TARGET_COL_NAMES)):
                name_j = TARGET_COL_NAMES[j]
                pair_names.append(f"{name_i}+{name_j}")
                idx += 1

        per_pair_acc: Dict[str, float] = {}
        for k, pname in enumerate(pair_names):
            if count_per_pair[k] > 0:
                per_pair_acc[pname] = float(correct_per_pair[k] / count_per_pair[k])
            else:
                per_pair_acc[pname] = float("nan")  # no samples for this pair

        cls_metrics = ClassificationMetrics(
            top1_acc=float(acc_top1),
            macro_f1=macro_f1,
            per_pair_acc=per_pair_acc,
        )

        # ===============================
        #   Regression metrics
        # ===============================
        rmse_per_dim = torch.sqrt(sum_sq_err / total_samples)  # [6]
        mae_per_dim = sum_abs_err / total_samples  # [6]

        rmse_overall = float(torch.sqrt((sum_sq_err / total_samples).mean()).item())
        mae_overall = float(mae_per_dim.mean().item())

        rmse_per_dim_dict = {
            name: float(val)
            for name, val in zip(TARGET_COL_NAMES, rmse_per_dim.detach().cpu())
        }
        mae_per_dim_dict = {
            name: float(val)
            for name, val in zip(TARGET_COL_NAMES, mae_per_dim.detach().cpu())
        }

        reg_metrics = RegressionMetrics(
            rmse_overall=rmse_overall,
            mae_overall=mae_overall,
            rmse_per_dim=rmse_per_dim_dict,
            mae_per_dim=mae_per_dim_dict,
        )

        # ===============================
        #   Final combined metrics object
        # ===============================
        return MultiTaskMetrics(
            loss_total=float(loss_total),
            loss_cls=float(loss_cls),
            loss_reg=float(loss_reg),
            classification=cls_metrics,
            regression=reg_metrics,
        )
