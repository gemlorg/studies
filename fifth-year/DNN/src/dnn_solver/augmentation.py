from dnn_solver.constants import (
    TRIANGLE_DOWN_IDX,
    TRIANGLE_LEFT_IDX,
    TRIANGLE_RIGHT_IDX,
    TRIANGLE_UP_IDX,
)
from dnn_solver.types import DataAugmentationConfig
from torchvision.transforms.v2 import functional as F
import torch
import torch.nn as nn
from typing import List, Tuple
import math


class GuassianNoiseAugmentation:
    def __init__(
        self, mean: float = 0.0, stddev: float = 0.1, probability: float = 0.3
    ):
        self.mean = mean
        self.stddev = stddev
        self.probability = probability

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        if torch.rand(()) < self.probability:
            noise = torch.randn_like(img) * self.stddev + self.mean
            img = img + noise
            img = torch.clamp(img, 0.0, 1.0)
        return img, target


class BrightnessContrastAugmentation:
    def __init__(self, brightness: float = 0.1, contrast: float = 0.1, probability: float = 0.3):
        self.brightness = brightness
        self.contrast = contrast
        self.probability = probability

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        if torch.rand(()) < self.probability:
            b = 1.0 + float(torch.empty(()).uniform_(-self.brightness, self.brightness))
            c = 1.0 + float(torch.empty(()).uniform_(-self.contrast, self.contrast))
            img = F.adjust_brightness(img, b)
            img = F.adjust_contrast(img, c)
            img = torch.clamp(img, 0.0, 1.0)
        return img, target


class RandomDeleteAugmentation:
    """
    Random erasing of a small rectangle.
    """

    def __init__(
        self,
        probability: float = 0.3,
        scale: Tuple[float, float] = (0.02, 0.1),
        ratio: Tuple[float, float] = (0.3, 3.3),
    ):
        self.probability = probability
        self.scale = scale
        self.ratio = ratio

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        if torch.rand(()) >= self.probability:
            return img, target

        _, h, w = img.shape
        area = h * w
        target_area = float(torch.empty(()).uniform_(self.scale[0], self.scale[1])) * area
        log_ratio = (math.log(self.ratio[0]), math.log(self.ratio[1]))
        aspect = math.exp(float(torch.empty(()).uniform_(log_ratio[0], log_ratio[1])))

        erase_h = int(round(math.sqrt(target_area * aspect)))
        erase_w = int(round(math.sqrt(target_area / aspect)))

        if erase_h < 1 or erase_w < 1 or erase_h >= h or erase_w >= w:
            return img, target

        top = int(torch.randint(0, h - erase_h + 1, ()).item())
        left = int(torch.randint(0, w - erase_w + 1, ()).item())
        img = img.clone()
        img[:, top : top + erase_h, left : left + erase_w] = 0.0
        return img, target


class FlipAugmentation:
    def __init__(
        self, horizontal: bool = True, vertical: bool = True, probability: float = 0.3
    ):
        self.horizontal = horizontal
        self.vertical = vertical
        self.probability = probability

    def _vertical_flip(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        img = F.vflip(img)
        # swap up and down
        tmp = target.clone()
        (target[TRIANGLE_UP_IDX], target[TRIANGLE_DOWN_IDX]) = (
            tmp[TRIANGLE_DOWN_IDX],
            tmp[TRIANGLE_UP_IDX],
        )
        return img, target

    def _horizontal_flip(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        img = F.hflip(img)
        # swap left and right
        tmp = target.clone()
        (target[TRIANGLE_LEFT_IDX], target[TRIANGLE_RIGHT_IDX]) = (
            tmp[TRIANGLE_RIGHT_IDX],
            tmp[TRIANGLE_LEFT_IDX],
        )
        return img, target

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        if self.vertical and torch.rand(()) < self.probability:
            img, target = self._vertical_flip(img, target)
        if self.horizontal and torch.rand(()) < self.probability:
            img, target = self._horizontal_flip(img, target)
        return img, target


class RotationAugmentation:
    def __init__(self, probability: float = 0.3):
        self.probability = probability

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        if torch.rand(()) < self.probability:
            angles = torch.tensor([90, 180, 270], device=img.device)
            angle = angles[torch.randint(0, len(angles), ()).item()].item()
            img = F.rotate(img, angle)
            # update target accordingly
            while angle > 0:
                tmp = target.clone()
                (
                    target[TRIANGLE_UP_IDX],
                    target[TRIANGLE_RIGHT_IDX],
                    target[TRIANGLE_DOWN_IDX],
                    target[TRIANGLE_LEFT_IDX],
                ) = (
                    tmp[TRIANGLE_RIGHT_IDX],
                    tmp[TRIANGLE_DOWN_IDX],
                    tmp[TRIANGLE_LEFT_IDX],
                    tmp[TRIANGLE_UP_IDX],
                )
                angle -= 90
        return img, target


class DataAugmentation:
    def __init__(self, config: DataAugmentationConfig = DataAugmentationConfig()):
        self.augmentations: List[nn.Module] = []
        if config.flip:
            self.augmentations.append(
                FlipAugmentation(
                    horizontal=config.flip.horizontal,
                    vertical=config.flip.vertical,
                    probability=config.flip.probability,
                )
            )
        if config.rotation:
            self.augmentations.append(
                RotationAugmentation(probability=config.rotation.probability)
            )
        if config.gaussian_noise:
            self.augmentations.append(
                GuassianNoiseAugmentation(
                    mean=config.gaussian_noise.mean,
                    stddev=config.gaussian_noise.stddev,
                    probability=config.gaussian_noise.probability,
                )
            )
        if config.brightness_contrast:
            self.augmentations.append(
                BrightnessContrastAugmentation(
                    brightness=config.brightness_contrast.brightness,
                    contrast=config.brightness_contrast.contrast,
                    probability=config.brightness_contrast.probability,
                )
            )
        if config.random_delete:
            self.augmentations.append(
                RandomDeleteAugmentation(
                    probability=config.random_delete.probability,
                    scale=config.random_delete.scale,
                    ratio=config.random_delete.ratio,
                )
            )

    def __call__(
        self, img: torch.Tensor, target: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        img = img.clone()
        target = target.clone()
        for aug in self.augmentations:
            img, target = aug(img, target)
        return img, target
