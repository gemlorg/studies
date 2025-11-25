import logging
from dnn_solver.constants import (
    CSV_LABELS_FILENAME,
    IMG_COL_NAME,
    TARGET_COL_NAMES,
    TOTAL_DATASET_LEN,
    TRAIN_DATASET_LEN,
)
import torch
from torchvision import transforms
import os
import pandas as pd
from PIL import Image
from pathlib import Path
from typing import Callable
from torchvision.datasets.vision import VisionDataset
import matplotlib.pyplot as plt

logger = logging.getLogger("dnn_solver")

class IMGDataset(VisionDataset):
    def __init__(
        self,
        root: str | Path = "./data",
        train: bool | None = True,
        transform: Callable | None = None,
    ) -> None:
        super().__init__(root, transform=transform)
        self.train = train  # training set or test set

        labels_df = pd.read_csv(os.path.join(root, CSV_LABELS_FILENAME))
        assert (
            len(labels_df) == TOTAL_DATASET_LEN
        ), f"Expected {TOTAL_DATASET_LEN} samples, got {len(labels_df)}"

        if self.train is not None:
            if self.train:
                labels_df = labels_df.iloc[:TRAIN_DATASET_LEN]
            else:
                labels_df = labels_df.iloc[TRAIN_DATASET_LEN:]

        self.data, self.targets_raw = self._load_data(labels_df)
        self.labels = labels_df

    def _load_data(self, labels_df: pd.DataFrame) -> tuple[torch.Tensor, torch.Tensor]:
        imgs = []
        for img_path in labels_df[IMG_COL_NAME]:
            img = Image.open(os.path.join(self.root, img_path)).convert("L")
            tensor_img = transforms.ToTensor()(img)  # [1, 28, 28]
            imgs.append(tensor_img)

        data = torch.stack(imgs, dim=0)  # [N, 1, 28, 28]
        logger.info("Preloaded data shape: " + str(data.shape))
        targets = torch.tensor(
            labels_df[TARGET_COL_NAMES].values, dtype=torch.float32
        )  # [N, 6]

        return data, targets

    def __len__(self) -> int:
        return self.data.shape[0]

    def __getitem__(self, index: int) -> tuple[torch.Tensor, torch.Tensor]:
        img = self.data[index]  # [1, 28, 28]
        target = self.targets_raw[index]  # [6]

        if self.transform is not None and self.train:
            img, target = self.transform(img, target)

        return img, target

    def repr_item(self, index: int) -> None:
        """Show the image and print the target labels."""
        img, target = self[index]
        img_np = img.squeeze(0).detach().cpu().numpy()  # [28, 28]
        plt.imshow(img_np)
        plt.axis("off")
        base_info = f"index={index}, targets: "
        label_str = base_info + "\n"
        for name, label in zip(TARGET_COL_NAMES, target):
            label_str += f"{name}: {label.item():.0f} "
        plt.title(f"{label_str}")
        plt.show()

    def get_labels(self) -> pd.DataFrame:
        """Return the labels DataFrame."""
        return self.labels
