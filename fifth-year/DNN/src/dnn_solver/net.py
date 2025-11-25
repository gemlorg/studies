from __future__ import annotations
import torch
import torch.nn as nn

class GSNMultiTaskNet(nn.Module):

    def __init__(
        self, num_classes: int = 135, num_counts: int = 6, dropout_p: float = 0.3
    ):
        super().__init__()
        self.num_classes = num_classes
        self.num_counts = num_counts

        self.backbone = nn.Sequential(
            nn.Conv2d(1, 8, 3, stride=1, padding=1),
            nn.ReLU(),
            nn.Conv2d(8, 16, 3, stride=1, padding=1),
            nn.ReLU(),
            nn.Conv2d(16, 32, 3, stride=1, padding=1),
            nn.ReLU(),
            nn.Conv2d(32, 64, 3, stride=1, padding=1),
            nn.ReLU(),
            nn.Flatten(start_dim=1),
            nn.Linear(64 * 28 * 28, 256),
            nn.ReLU(),
        )

        self.head_cls = nn.Sequential(
            nn.Linear(256, 256),
            nn.ReLU(),
            nn.Dropout(p=dropout_p),
            nn.Linear(256, num_classes),
            nn.LogSoftmax(dim=1),
        )

        self.head_cnt = nn.Sequential(
            nn.Linear(256, 128),
            nn.ReLU(),
            nn.Dropout(p=dropout_p / 2),
            nn.Linear(128, num_counts),
        )

    def forward(self, x: torch.Tensor):
        features = self.backbone(x)  # [B, 256]
        log_probs = self.head_cls(features)  # [B, num_classes]
        counts = self.head_cnt(features)  # [B, 6]
        return log_probs, counts
