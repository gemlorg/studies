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
import torch.nn as nn

from models import Ritz
from utils import sampler, metrics
from mpl_toolkits.axes_grid1 import make_axes_locatable
from data_provider.data_factory import get_problem

import matplotlib.pyplot as plt
from utils.tools import weights_init

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

fix_seed = 42 
random.seed(fix_seed)
torch.manual_seed(fix_seed)
np.random.seed(fix_seed)


args = get_args()
args.alpha = 4
args.beta = 45
args.problem = 'laplace'


args.m = 10

args.lr = 3e-4

args.interior_n = 300
args.boundary_n = 100
args.epochs = 30000
args.depth = 4


sampler, criterion, solution = get_problem(args)
model = Ritz.Ritz(args.in_N, args.m, args.out_N, depth=args.depth).to(device)
weights_init(model)


optimizer = optim.Adam(model.parameters(), lr=args.lr)

mse_metric = nn.MSELoss()
sup_metric = nn.L1Loss()

for i in range(args.epochs):
    x_int, x_bdy = sampler.sample(args.interior_n, args.boundary_n)
    # show the points
    # plt.scatter(x_int[:, 0], x_int[:, 1], c='blue', s=0.1)
    # plt.scatter(x_bdy[:, 0], x_bdy[:, 1], c='red', s=0.1)
    # plt.show()

    x_int = x_int.to(device)
    x_bdy = x_bdy.to(device)

    x_int.requires_grad = True

    y_int = model(x_int)
    y_bdy = model(x_bdy)

    loss = criterion(x_int, x_bdy, y_int, y_bdy)

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    if i % 1000 == 0:
        if args.has_analytic_solution:
            expected_x = solution(x_int)
            expected_y = solution(x_bdy)
            expected_output = torch.cat([expected_x, expected_y], dim=0).to(device)
            expected_output = expected_output.unsqueeze(1)

            model_output = torch.cat([y_int, y_bdy], dim=0)
            mse = mse_metric(model_output, expected_output)
            sup = sup_metric(model_output, expected_output)
            print("Epoch: {}, Loss: {}, MSE: {}, Sup: {}".format(i, loss.item(), mse.item(), sup.item()))
        else:
            print("Epoch: {}, Loss: {}".format(i, loss.item()))



# print the output of the trained model

x_int, x_bdy = sampler.sample(10000, 1000)

# print(x_int)

X = torch.cat([x_int, x_bdy], dim=0)
with torch.no_grad():
    pred = model(X)

pred = pred.cpu().numpy()

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
# ax.plot_surface(x_np, y_np, z_np, cmap='viridis')
ax.scatter(X[:, 0], X[:, 1], pred, c='red', s=0.1)

analytic = solution(X) 
ax.scatter(X[:, 0], X[:, 1], analytic, c='blue', s=0.1)

ax.set_xlabel('X axis')
ax.set_ylabel('Y axis')
ax.set_zlabel('Z axis')
plt.show()

with torch.no_grad():
        x1 = torch.linspace(-1, 1, 1001)
        x2 = torch.linspace(-1, 1, 1001)
        X, Y = torch.meshgrid(x1, x2)
        Z = torch.cat((Y.flatten()[:, None], Y.T.flatten()[:, None]), dim=1)
        # if 2 < m:
        #     y = torch.zeros(Z.shape[0], m - 2)
        #     Z = torch.cat((Z, y), dim=1)
        Z = Z.to(device)

        pred = model(Z)
        # pred = solution(Z)
        # pred = pred.unsqueeze(1)
plt.figure()
pred = pred.reshape(1001, 1001)
ax = plt.subplot(1, 1, 1)
h = plt.imshow(pred, interpolation='nearest', cmap='rainbow',
                   extent=[-1, 1, -1, 1],
                   origin='lower', aspect='auto')
divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="5%", pad=0.05)
plt.colorbar(h, cax=cax)
plt.show()






