from copy import deepcopy
from dnn_solver.constants import TARGET_COL_NAMES
from dnn_solver.loss import GSNMultiTaskLoss
from dnn_solver.types import (
    ClassificationMetrics,
    MultiTaskLossOutput,
    MultiTaskMetrics,
    RegressionMetrics,
)
from dnn_solver.utils import counts_to_config_ids
import torch
import torch.nn as nn
from torch.utils.data import DataLoader
from tqdm import tqdm
import matplotlib.pyplot as plt
import os

# Allow custom (non-ipywidget) widgets.


from typing import Dict, List, Optional, Tuple


class MultiTaskTrainer:

    def __init__(
        self,
        model: nn.Module,
        trainloader: DataLoader,
        testloader: DataLoader,
        device: torch.device,
        lambda_cnt: float,
        use_cls_loss: bool = True,
        values_min: int = 1,
        values_max: int = 9,
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
        self.train_epoch_losses: List[MultiTaskLossOutput] = []

    def train(
        self,
        epochs: int = 100,
        lr: float = 1e-3,
        weight_decay: float = 0.0,
        log_freq: int = 50,
        patience: int = 5,
    ) -> MultiTaskMetrics:
        optimizer = torch.optim.Adam(
            self.model.parameters(),
            lr=lr,
            weight_decay=weight_decay,
        )
        curr_patience = patience
        metrics: Optional[MultiTaskMetrics] = None
        for epoch in range(epochs):
            self.model.train()
            running_loss: MultiTaskLossOutput = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)
            total_samples = 0
            sum_epoch_loss: MultiTaskLossOutput = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)
            epoch_samples = 0

            for batch_idx, (images, count_targets) in enumerate(self.trainloader):
                images = images.to(self.device)
                count_targets = count_targets.to(self.device)

                optimizer.zero_grad()
                log_probs, counts_pred = self.model(
                    images
                )
                full_loss = self.criterion(log_probs, counts_pred, count_targets)
                loss_detached = full_loss.detach()
                self.loss_history.append(loss_detached)

                loss = full_loss.total
                loss.backward()
                optimizer.step()

                running_loss += loss_detached
                bs = images.size(0)
                total_samples += bs
                epoch_samples += bs
                sum_epoch_loss += loss_detached.scale(bs)


                if (batch_idx + 1) % log_freq == 0:
                    avg_loss = running_loss.total.item() / log_freq
                    print(
                        f"epoch {epoch + 1:>4}, "
                        f"batch {batch_idx + 1:>5}, "
                        f"loss {avg_loss:.3f}"
                    )
                    running_loss = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)

            # end of epoch: evaluate on test set
            if epoch_samples > 0:
                avg_epoch_loss = sum_epoch_loss.scale(1.0 / epoch_samples)
                self.train_epoch_losses.append(avg_epoch_loss)
            else:
                self.train_epoch_losses.append(MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0))

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
                break

        # summarize and return best metrics
        final_metrics = self.best_eval 
        if final_metrics is None:
            raise RuntimeError("Training finished without any evaluation metrics.")
        self._print_final_metrics(final_metrics)
        return final_metrics

    def history(self) -> List[Tuple[MultiTaskLossOutput, MultiTaskMetrics]]:
        return list(zip(self.train_epoch_losses, self.eval_history))

    def plot_learning_curves(self, save_dir: Optional[str] = None) -> None:
        if not self.eval_history:
            raise RuntimeError("No evaluation history to plot. Run train() first.")

        epochs = list(range(1, len(self.eval_history) + 1))
        name_suffix = f"lambda{self.lambda_cnt}_cls{int(self.use_cls_loss)}_reg{int(self.lambda_cnt != 0)}"

        to_float = lambda x: float(x.detach().cpu()) if hasattr(x, "detach") else float(x)
        train_totals = [to_float(e.total) for e in self.train_epoch_losses]
        train_cls = [to_float(e.cls) for e in self.train_epoch_losses]
        train_reg = [to_float(e.reg) for e in self.train_epoch_losses]
        val_totals = [to_float(m.loss_total) for m in self.eval_history]
        plt.figure(figsize=(8, 4))
        plt.plot(epochs, train_totals, label="train loss")
        plt.plot(epochs, val_totals, label="val loss")
        plt.plot(epochs, train_cls, label="train cls loss")
        plt.plot(epochs, train_reg, label="train reg loss")
        plt.xlabel("Epoch")
        plt.ylabel("Loss")
        plt.title("Training/Validation Loss")
        plt.legend()
        if save_dir:
            os.makedirs(save_dir, exist_ok=True)
            plt.savefig(os.path.join(save_dir, f"loss_curves_{name_suffix}.png"), bbox_inches="tight")
        plt.show()
        if save_dir:
            plt.close()

        # --- Accuracy curve ---
        val_acc = [m.classification.top1_acc for m in self.eval_history]
        plt.figure(figsize=(8, 4))
        plt.plot(epochs, val_acc, label="val top1 acc")
        plt.xlabel("Epoch")
        plt.ylabel("Accuracy")
        plt.title("Validation Accuracy")
        plt.legend()
        if save_dir:
            plt.savefig(os.path.join(save_dir, f"val_accuracy_{name_suffix}.png"), bbox_inches="tight")
        plt.show()
        if save_dir:
            plt.close()

        # --- RMSE curve ---
        val_rmse = [m.regression.rmse_overall for m in self.eval_history]
        plt.figure(figsize=(8, 4))
        plt.plot(epochs, val_rmse, label="val RMSE")
        plt.xlabel("Epoch")
        plt.ylabel("RMSE")
        plt.title("Validation RMSE")
        plt.legend()
        if save_dir:
            os.makedirs(save_dir, exist_ok=True)
            plt.savefig(os.path.join(save_dir, f"val_rmse_{name_suffix}.png"), bbox_inches="tight")
        plt.show()
        if save_dir:
            plt.close()

    def _print_final_metrics(self, metrics: MultiTaskMetrics) -> None:
        """Pretty-print the final (best) metrics."""
        print("\n=== Final (best) metrics ===")
        print(metrics)
        # worst 5 pairs by accuracy (ascending), ignoring NaNs
        pair_acc = [
            (p, a) for p, a in metrics.classification.per_pair_acc.items()
        ]
        pair_acc.sort(key=lambda x: x[1])
        worst_pairs = pair_acc[:5]
        print("Worst pairs (acc):", {p: round(a, 4) for p, a in worst_pairs})

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

        # --------- Accumulators ----------
        total_samples = 0
        sum_loss = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)

        pair_names = []
        num_values = self.values_max - self.values_min + 1
        for i, name_i in enumerate(TARGET_COL_NAMES):
            for j in range(i + 1, len(TARGET_COL_NAMES)):
                name_j = TARGET_COL_NAMES[j]
                pair_names.append(f"{name_i}+{name_j}")

        cls_metrics = ClassificationMetrics.accumulator(
            pair_names=pair_names,
            num_values=num_values,
            values_min=self.values_min,
            values_max=self.values_max,
        )
        reg_metrics = RegressionMetrics.accumulator(
            target_names=TARGET_COL_NAMES, device=self.device
        )

        for images, count_targets in self.testloader:
            images = images.to(self.device)
            count_targets = count_targets.to(self.device)

            log_probs, counts_pred = self.model(images)  # [B, C], [B, 6]
            batch_size = images.size(0)
            total_samples += batch_size

            # ----- Losses -----
            loss_out = self.criterion(log_probs, counts_pred, count_targets)
            sum_loss += loss_out.scale(batch_size)
            
            cls_metrics.add_batch(log_probs, counts_pred, count_targets)
            reg_metrics.add_batch(counts_pred, count_targets)

        if total_samples == 0:
            raise RuntimeError("No samples in testloader.")

        sum_loss = sum_loss.scale(1.0 / total_samples)

        return MultiTaskMetrics(
            loss=sum_loss,
            classification=cls_metrics.aggregate(),
            regression=reg_metrics.aggregate(),
        )
