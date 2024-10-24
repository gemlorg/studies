from data_provider.data_loader import *
from utils.metrics import IntegralLoss

problem_dict = {
    'pois2d' : Poisson2D,
    'laplace' : LaplaceEq,

}
def get_problem(args):
    problem = problem_dict[args.problem]()
    loss = IntegralLoss(problem.loss_int_f, problem.bdy_f, alpha = args.alpha, beta = args.beta)
    args.in_N = problem.in_N
    args.out_N = problem.out_N
    args.has_analytic_solution = problem.has_analytic_solution
    return problem.sampler, loss, problem.solution