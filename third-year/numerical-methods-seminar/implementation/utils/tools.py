import torch
from torch import nn, optim
from torch.optim import lr_scheduler
from tqdm import tqdm
from models import Ritz
import time
import random
import numpy as np
import os
import json
import csv
from utils.config_parser import get_args
import torch.autograd as autograd

from models import Ritz
from utils import sampler, metrics
from mpl_toolkits.axes_grid1 import make_axes_locatable
from data_provider.data_factory import get_problem

import matplotlib.pyplot as plt


def weights_init(m):
    if isinstance(m, (nn.Conv2d, nn.Linear)):
        nn.init.xavier_normal_(m.weight)
        nn.init.constant_(m.bias, 0.0)