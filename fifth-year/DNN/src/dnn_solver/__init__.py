import logging
import sys
from .augmentation import DataAugmentation
from .dataset import IMGDataset
from .loss import GSNMultiTaskLoss
from .net import GSNMultiTaskNet
from .trainer import MultiTaskTrainer


logger = logging.getLogger("dnn-solver")
logger.setLevel(logging.INFO)
logger.propagate = False
handler = logging.StreamHandler(sys.stdout)
handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
logger.addHandler(handler)


__all__ = [
    "DataAugmentation",
    "IMGDataset",
    "GSNMultiTaskLoss",
    "GSNMultiTaskNet",
    "MultiTaskTrainer",
]
