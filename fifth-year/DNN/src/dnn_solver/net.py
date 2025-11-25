from __future__ import annotations
import torch
import torch.nn as nn
import torch.nn.functional as F


class GSNMultiTaskNet(nn.Module):
    """
    Multitask model for GSN:
      - Classification: 135 config classes -> log-probs
      - Regression: 6 shape counts

    Input:  x  [B, 1, 28, 28]
    Output: (log_probs, counts)
      - log_probs: [B, 135]
      - counts:    [B, 6]
    """

    def __init__(
        self, num_classes: int = 105, num_counts: int = 6, dropout_p: float = 0.3
    ):
        super().__init__()
        self.num_classes = num_classes
        self.num_counts = num_counts

        # ---------- fixed backbone (do NOT change) ----------
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

        # ---------- classification head: 135-way log-probs ----------
        self.head_cls = nn.Sequential(
            nn.Linear(256, 128), nn.ReLU(),
            nn.Dropout(p=dropout_p),
            nn.Linear(128, num_classes)
        )

        # ---------- regression head: 6 counts ----------
        self.head_cnt = nn.Sequential(
            nn.Linear(256, 64),
            nn.ReLU(),
            nn.Dropout(p=dropout_p/2),
            # Dropout is less common in regression, but small dropout (0.2) is okay. 
            # Here we skip it to keep the signal clean for counts.
            nn.Linear(64, num_counts)
        )

    def forward(self, x: torch.Tensor):
        """
        x: [B, 1, 28, 28]
        returns:
          log_probs: [B, num_classes]
          counts:    [B, num_counts]
        """
        features = self.backbone(x)  # [B, 256]

        logits = self.head_cls(features)  # [B, 105]

        counts = self.head_cnt(features)  # [B, 6]

        return logits, counts
