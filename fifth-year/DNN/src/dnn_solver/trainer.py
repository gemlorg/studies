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

            for batch_idx, (images, count_targets) in enumerate(self.trainloader):
                images = images.to(self.device)
                count_targets = count_targets.to(self.device)

                optimizer.zero_grad()
                log_probs, counts_pred = self.model(
                    images
                )  # model must return (cls_logits, counts_pred)
                full_loss = self.criterion(log_probs, counts_pred, count_targets)
                self.loss_history.append(full_loss)

                loss = full_loss.total
                loss.backward()

                optimizer.step()

                running_loss += loss

                bs = images.size(0)
                total_samples += bs


                if (batch_idx + 1) % log_freq == 0:
                    avg_loss = running_loss.total.item() / log_freq
                    print(
                        f"epoch {epoch + 1:>4}, "
                        f"batch {batch_idx + 1:>5}, "
                        f"loss {avg_loss:.3f}"
                    )
                    running_loss = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)

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
                break

        # summarize and return best metrics
        final_metrics = self.best_eval 
        if final_metrics is None:
            raise RuntimeError("Training finished without any evaluation metrics.")
        self._print_final_metrics(final_metrics)
        return final_metrics

    def history(self) -> List[Tuple[MultiTaskLossOutput, MultiTaskMetrics]]:
        """
        Returns recorded training and evaluation history for downstream analysis or plotting.
        - train_epoch_history: list of dicts with loss_total/loss_cls/loss_reg per epoch
        - eval_history: list of MultiTaskMetrics per epoch
        """
        return list(zip(self.loss_history, self.eval_history))

    def plot_learning_curves(self, save_dir: Optional[str] = None) -> None:
        """
        Plot training/validation losses, validation accuracy, and validation RMSE over epochs.
        If save_dir is provided, figures are saved there; otherwise they are shown inline.
        """

        if not self.eval_history:
            raise RuntimeError("No evaluation history to plot. Run train() first.")

        epochs = list(range(1, len(self.eval_history) + 1))

        # --- Loss curves ---
        train_totals = [e.total for e in self.loss_history]
        train_cls = [e.cls for e in self.loss_history]
        train_reg = [e.reg for e in self.loss_history]
        val_totals = [m.loss_total for m in self.eval_history]
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
            plt.savefig(os.path.join(save_dir, "loss_curves.png"), bbox_inches="tight")
            plt.close()
        else:
            plt.show()

        # --- Accuracy curve ---
        val_acc = [m.classification.top1_acc for m in self.eval_history]
        plt.figure(figsize=(8, 4))
        plt.plot(epochs, val_acc, label="val top1 acc")
        plt.xlabel("Epoch")
        plt.ylabel("Accuracy")
        plt.title("Validation Accuracy")
        plt.legend()
        if save_dir:
            plt.savefig(os.path.join(save_dir, "val_accuracy.png"), bbox_inches="tight")
            plt.close()
        else:
            plt.show()

        # --- RMSE curve ---
        val_rmse = [m.regression.rmse_overall for m in self.eval_history]
        plt.figure(figsize=(8, 4))
        plt.plot(epochs, val_rmse, label="val RMSE")
        plt.xlabel("Epoch")
        plt.ylabel("RMSE")
        plt.title("Validation RMSE")
        plt.legend()
        if save_dir:
            plt.savefig(os.path.join(save_dir, "val_rmse.png"), bbox_inches="tight")
            plt.close()
        else:
            plt.show()

    def _print_final_metrics(self, metrics: MultiTaskMetrics) -> None:
        """Pretty-print the final (best) metrics."""
        print("\n=== Final (best) metrics ===")
        print(
            f"Losses -> total: {metrics.loss_total:.4f}, "
            f"cls: {metrics.loss_cls:.4f}, reg: {metrics.loss_reg:.4f}"
        )
        cls = metrics.classification
        print(
            f"Classification -> top1: {cls.top1_acc:.4f}, macro F1: {cls.macro_f1:.4f}"
        )
        reg = metrics.regression
        print(
            f"Regression -> RMSE overall: {reg.rmse_overall:.4f}, "
            f"MAE overall: {reg.mae_overall:.4f}"
        )
        print("RMSE per class:", {k: round(v, 4) for k, v in reg.rmse_per_dim.items()})
        print("MAE  per class:", {k: round(v, 4) for k, v in reg.mae_per_dim.items()})

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
        sum_loss = MultiTaskLossOutput(total=0.0, cls=0.0, reg=0.0)
        sum_cls_metrics = ClassificationMetrics()
        sum_reg_metrics = RegressionMetrics()

        # --------- Classification metrics ----------
        correct_top1 = 0
        all_true = []  # list of 1D tensors with true config IDs
        all_pred = []  # list of 1D tensors with predicted config IDs


        for images, count_targets in self.testloader:
            images = images.to(self.device)
            count_targets = count_targets.to(self.device)

            log_probs, counts_pred = self.model(images)  # [B, C], [B, 6]
            batch_size = images.size(0)
            total_samples += batch_size

            # ----- Losses -----
            loss_out = self.criterion(log_probs, counts_pred, count_targets)
            sum_loss += loss_out.scale(batch_size)
            
            sum_cls_metrics.add_batch(log_probs, counts_pred, count_targets)
            sum_reg_metrics.add_batch(log_probs, counts_pred, count_targets)

        if total_samples == 0:
            raise RuntimeError("No samples in testloader.")

        sum_loss = sum_loss.scale(1.0 / total_samples)
        sum_cls_metrics.aggregate()
        sum_reg_metrics.aggregate()

        return MultiTaskMetrics(
            loss=sum_loss,
            classification=sum_cls_metrics,
            regression=sum_reg_metrics,
        )
