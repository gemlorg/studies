
import torch
import torch.nn as nn
import pandas as pd
import numpy as np
from typing import Callable
from torch import autograd 

from os.path import join

from utils.sampler import Interval, Sampler

class Problem: 
    """
    Abstract class for a problem.
    the get_problem method should return a tuple of the form (s, f, u) where:
    - s is a Sampler object
    - f is the loss function    
    - u is the analytic solution of the problem(if available)
    """
    def __init__(self):
        pass
    

class DirichletProblem(Problem):
    def __init__(self, sampler, int_f, bdy_f, in_N, out_N=1, solution=1, has_analytic_solution=False):
        assert(out_N == 1) 

        def loss_int_f(x_int, y_int):
            grads = autograd.grad(outputs=y_int, inputs=x_int,
                              grad_outputs=torch.ones_like(y_int),
                              create_graph=True, retain_graph=True, only_inputs=True)[0]
            loss_int = 0.5 * torch.sum(torch.pow(grads, 2),dim=1) -  int_f(x_int) * y_int
            loss_int = torch.mean(loss_int)
            return loss_int
        
        self.in_N = in_N
        self.out_N = out_N
        self.has_analytic_solution = has_analytic_solution
        self.loss_int_f = loss_int_f
        self.bdy_f = bdy_f
        self.solution = solution
        self.sampler = sampler



class Poisson2D(DirichletProblem):
    def __init__(self):
        def equation(points: torch.Tensor) -> torch.Tensor:
            # points: Tensor of shape (n, d), where n is the number of points and d is the dimension of each point
            # Ω = (−1, 1) × (−1, 1)\[0, 1) × {0}
            conditions_met = torch.all((points >= -1) & (points <= 1), dim=1)
            results = torch.where(conditions_met, torch.tensor(1), torch.tensor(-1))
            return results
        def boundary_path(t):
            t = 5 * t
            if t < 1:
                return torch.tensor([-1, -1 + 2 * t])
            elif t < 2:
                return torch.tensor([-1 + 2 * (t - 1), 1])
            elif t < 3:
                return torch.tensor([1, 1 - 2 * (t - 2)])
            elif t < 4:
                return torch.tensor([1 - 2 * (t - 3), -1])
            else:
                return torch.tensor([t - 4, 0])
        def solution(vals):
            x, y = vals[:, 0], vals[:, 1]
            # Convert (x, y) to polar coordinates (r, theta)
            r = torch.sqrt(x * x + y * y)
            phi = torch.atan2(y, x)
            phi = torch.where(phi < 0, phi + 2 * np.pi, phi)

            
            # Apply the given function in polar coordinates)
            new_r = torch.sin(phi * 0.5)
            
            return new_r
        
        def bdy_f(x_bdy):
            # input is a torch tensor
            return torch.zeros(x_bdy.shape[0])
        def int_f(x_int):
            return torch.ones(x_int.shape[0])
        
        interval = Interval([-1, -1], [1, 1])
        sampler = Sampler(boundary_path, equation, interval)
        in_N = 2
        out_N = 1
        has_analytic_solution = False
        super().__init__(sampler, int_f, bdy_f, in_N, out_N, solution, has_analytic_solution)

class LaplaceEq(DirichletProblem):
    def __init__(self):
        # def equation(points):
        #     x = points[:, 0]
        #     y = points[:, 1]
        #     return - x * x - y * y +1
        # def boundary_path(t):
        #     t = 2 * torch.pi * t
        #     return torch.tensor([torch.cos(t), torch.sin(t)])
        def solution(vals):
            # r * sin(theta)
            x = vals[:, 0]
            y = vals[:, 1]
            # r = torch.sqrt(x * x + y * y)
            # theta = torch.atan2(y, x)
            return x * x * x + y * y * y - x * x 
        def int_f(x_bdy):
            x = x_bdy[:, 0]
            y = x_bdy[:, 1]

            return 6 * x + 6 * y - 2
        def bdy_f(x_bdy):
            x = x_bdy[:, 0]
            y = x_bdy[:, 1]
            return x * x * x + y * y * y - x * x 
        # interval = Interval([-1, -1], [1, 1])
        # sampler = Sampler(boundary_path, equation, interval)
        sampler = Sphere(1, 2)()
        in_N = 2
        out_N = 1
        has_analytic_solution = True 
        super().__init__(sampler, int_f, bdy_f, in_N, out_N, solution, has_analytic_solution)


        

class Sphere:
    def __init__(self, r, dim, center=0):
        self.r = r
        self.dim = dim
        self.center = center
    
    def __call__(self):
        def equation(points):
            x = points[:, 0]
            y = points[:, 1]
            return - x * x - y * y +self.r * self.r
        def boundary_path(t):
            t = 2 * torch.pi * t
            return self.r * torch.tensor([torch.cos(t), torch.sin(t)])
        
        interval = Interval( [-self.r, -self.r],  [self.r, self.r])
        return Sampler(boundary_path, equation, interval)
         


        
