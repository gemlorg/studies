import logging
import sys
from .augmentation import DataAugmentation
from .dataset import IMGDataset
from .loss import MultiTaskLoss
from .net import MultiTaskNet
from .trainer import MultiTaskTrainer


logger = logging.getLogger("dnn_solver")
logger.setLevel(logging.INFO)
logger.propagate = False
handler = logging.StreamHandler(sys.stdout)
handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
logger.addHandler(handler)


__all__ = [
    "DataAugmentation",
    "IMGDataset",
    "MultiTaskLoss",
    "MultiTaskNet",
    "MultiTaskTrainer",
]
