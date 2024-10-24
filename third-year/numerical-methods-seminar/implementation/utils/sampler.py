from functools import reduce
import torch
import torch.nn as nn
import pandas as pd
import numpy as np
from typing import Callable

class Interval:
    def __init__(self, a_list, b_list):
        assert len(a_list) == len(b_list)
        self.dim = len(a_list)
        self.a_list = torch.tensor(a_list)
        self.b_list = torch.tensor(b_list)
        self.ranges = self.b_list - self.a_list
        self.volume = reduce(lambda x, y: x*y, self.ranges)


class Sampler:
    def __init__(self, 
                 boundary_path: Callable[[float], torch.Tensor], 
                 equation: Callable[[torch.Tensor], float], 
                 interval: Interval):
        self.boundary_path = boundary_path
        self.equation = equation
        self.interval = interval
    
    def sample_interior(self, n:int, filling_parameter:int=10) -> torch.Tensor:
        """
        Uniformly and randomly sample points in the interior of the domain D.
        The domain is given by {x in R^d | f(x) > 0} where f is the equation function.

        Parameters:
                self.interval (Interval): Interval object that contains the domain of the equation.
                self.equation (function): Equation function that takes a d-dimensional point and returns a scalar.
                n (int): Number of random uniform samples to generate.
                filling_parameter (int): Number of points to generate to ensure that n points are sampled.
        """
        int_points = torch.rand(filling_parameter * n, self.interval.dim)
        int_points = self.interval.a_list + int_points*self.interval.ranges
        eq_int_points = self.equation(int_points)
        int_points = int_points[eq_int_points > 0]
        return int_points[:n]
    
    def sample_boundary(self, n:int) -> torch.Tensor:
        """
        Uniformly and randomly sample points on the image of the path function f: I -> R^d.
        
        Parameters:

            self.boundary_path (function): Path function that takes a scalar input from (0,1) and returns an d-dimensional point.
            n (int): Number of random uniform samples to generate.
        
        Returns:
            torch.Tensor: Tensor of uniformly and randomly sampled points on the path.
        """
        # remark: currently doesn't sample uniformly, unless given a simple path function

        t_values = torch.rand(n)
        path_points = torch.stack([self.boundary_path(t) for t in t_values])
        return path_points[:n]

    def sample(self, n_int, n_boundary):
        int_points = self.sample_interior(n_int)
        boundary_points = self.sample_boundary(n_boundary)
        return int_points, boundary_points

