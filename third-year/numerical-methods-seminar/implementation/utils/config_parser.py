import argparse


def get_args():
    parser = argparse.ArgumentParser(description='Ritz-Method')
    parser.add_argument('--dimension', type=int, default=1, help='dimension of the model')

    return parser.parse_args()