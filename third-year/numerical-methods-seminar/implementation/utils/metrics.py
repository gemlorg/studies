
from os.path import join
from math import sqrt
import torch
import torch.nn as nn
import pandas as pd
import torch.autograd as autograd

class IntegralLoss(nn.Module):
    def __init__(self, int_loss_f, bdy_f, alpha = 4, beta = 500):
        super(IntegralLoss, self).__init__()
        self.int_loss_f = int_loss_f
        self.bdy_f = bdy_f
        self.alpha = alpha
        self.beta = beta
    def forward(self, x_int, x_bdy, y_int, y_bdy):
        # grads = autograd.grad(outputs=y_int, inputs=x_int,
        #                       grad_outputs=torch.ones_like(y_int),
        #                       create_graph=True, retain_graph=True, only_inputs=True)[0]
        

        # loss_int = 0.5 * torch.sum(torch.pow(grads, 2),dim=1) -  self.f(x_int) * y_int
        # loss_int = torch.mean(loss_int)
        loss_int = self.int_loss_f(x_int, y_int)
        
        need_bdy = self.bdy_f(x_bdy).unsqueeze(1)

        loss_bdy = torch.mean(torch.pow(y_bdy - need_bdy ,2))
        loss = torch.abs(self.alpha * loss_int) + torch.abs(self.beta * loss_bdy)
        return loss



