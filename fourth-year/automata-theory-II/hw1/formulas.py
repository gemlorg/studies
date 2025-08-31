import random
class CNF:
    """
    Class to represent a Conjunctive Normal Form (CNF) formula.
    A CNF is a conjunction (AND) of disjunctions (OR) of literals.
    """
    def __init__(self, clauses):
        """
        Initialize the CNF with a list of clauses.
        Each clause is a list of literals, where a literal is either a variable or its negation.
        Example: [["x1", "NOT x2"], ["x3", "x4"]]
        """
        self.clauses = clauses

    def __repr__(self):
        """
        String representation of the CNF formula.
        Example: "(x1 OR NOT x2) AND (x3 OR x4)"
        """
        clause_strings = ["(" + " OR ".join(clause) + ")" for clause in self.clauses]
        return " AND ".join(clause_strings)


class Calculator:
    """
    Class to evaluate a CNF formula given variables and their truth assignments.
    """
    def __init__(self, variables):
        """
        Initialize the calculator with a CNF formula and a list of variable names.
        :param cnf: An instance of the CNF class.
        :param variables: A list of variable names (e.g., ["x1", "x2", "x3"]).
        """
        self.variables = variables

    def evaluate_clause(self, clause, assignment):
        """
        Evaluate a single clause against a truth assignment.
        :param clause: A list of literals (e.g., ["x1", "NOT x2"]).
        :param assignment: A dictionary mapping variables to their truth values (e.g., {"x1": 1, "x2": 0}).
        :return: True if the clause is satisfied, False otherwise.
        """
        for literal in clause:

            if literal.startswith("NOT "):
                var = literal[5:]  # Remove "NOT " to get the variable names
                if not assignment[int(var) - 1]:
                    return True
            else:
                if assignment[int(literal[1:]) - 1]:
                    return True
        return False

    def evaluate(self, assignment):
        """
        Evaluate the entire CNF formula against a truth assignment.
        :param assignment: A dictionary mapping variables to their truth values (e.g., {"x1": 1, "x2": 0}).
        :return: True if the CNF formula is satisfied, False otherwise.
        """
        for clause in self.cnf.clauses:
            if not self.evaluate_clause(clause, assignment):
                return False
        return True


# Example usage
# Define a CNF formula: (x1 OR NOT x2) AND (x3)
# cnf_formula = CNF(V[["x1", "NOT x2"], ["x3"]])
#
# # Define the variables
# variables = ["x1", "x2", "x3"]
#
# # Create a Calculator instance
# calculator = Calculator(cnf_formula, variables)
#
# # Test the calculator with different assignments
# assignments = [
#     {"x1": 1, "x2": 0, "x3": 1},  # True
#     {"x1": 0, "x2": 1, "x3": 1},  # True
#     {"x1": 0, "x2": 1, "x3": 0},  # False
# ]
#
# for assignment in assignments:
#     result = calculator.evaluate(assignment)
#     print(f"Assignment: {assignment}, CNF Result: {result}")
#     dd = assignment
from itertools import combinations, chain


def generate_clauses(literals):
    """
    Generate all possible clauses up to a given size.
    """
    clauses = []
    for i in range(2**len(literals)):
        clause = [] 
        j = i
        for var in literals: 
            if j % 2 == 0: 
                clause.append(f"NOT {var}")
            else: 
                clause.append(var)
            j = j // 2 
        clauses.append(clause)

    # for size in range(1, max_clause_size + 1):
    #     clauses.extend(combinations(literals, size))
    return clauses

def generate_cnf_formulas(clauses):
    """
    Generate all possible CNF formulas with a given maximum number of clauses.
    """
    cnf_formulas = []
    for size in range(1, len(clauses) +  1):
        cnf_formulas.extend(combinations(clauses, size))
    return cnf_formulas

# Define variables
variables = [f"x{i+1}" for i in range(4)]

# Generate literals

# Generate clauses (limit clause size to 3 literals for simplicity)
clauses = generate_clauses(variables)
# print(clauses)

# Generate CNF formulas (limit total clauses to 3 for simplicity)
cnf_formulas = generate_cnf_formulas(clauses )

# Display a few CNF formulas
# print("Number of CNF formulas generated:", len(cnf_formulas))
# for i, cnf in enumerate(cnf_formulas[1000:1010]):
#     formula = " AND ".join(["(" + " OR ".join(clause) + ")" for clause in cnf])
#     print(f"CNF Formula {i+1}: {formula}")

def generate_random_rational_pair():
    # Generate two random float numbers
    x1 = random.uniform(0, 100)  # random float between 0 and 100
    x2 = random.uniform(0, 100)  # random float between 0 and 100
    
    # Ensure x1 is smaller than x2
    while x1 >= x2:
        x1 = random.uniform(0, 100)
        x2 = random.uniform(0, 100)
    
    return (x1, x2)

# Function to compare two pairs of rational numbers and return the list of comparisons
def compare_rational_pairs(pair1, pair2):
    # Unpack the pairs
    x1, x2 = pair1
    y1, y2 = pair2
    
    # Generate the list of comparisons
    comparison_result = [
        x1 <= x2,  # x1 <= x2
        x1 <= y2,  # x1 <= y2
        y1 <= x2,  # y1 <= x2
        y1 <= y2   # y1 <= y2
    ]
    
    return comparison_result

def comp_rand():
    pair1 = generate_random_rational_pair()
    pair2 = generate_random_rational_pair()
    return compare_rational_pairs(pair1, pair2)

def comp_rand_same():
    pair1 = generate_random_rational_pair()
    pair2 = pair1
    return compare_rational_pairs(pair1, pair2)

calculator = Calculator( variables) 
import math
def compare(p1, p2):
    return calculator.evaluate(compare_rational_pairs(p1, p2))
def equals(p1, p2):
    return abs(p1[0] - p2[0]) < 0.0001 and abs(p1[1] - p2[1]) < 0.0001
def good_order(formula):
    calculator.cnf = CNF(formula)
    num_test = 10

    for i in range(num_test):
        p1 = generate_random_rational_pair() 
        p2 = generate_random_rational_pair() 
        p3 = generate_random_rational_pair() 

        # reflexivity 
        if not compare(p1, p1): return False


       
        # transitivity  
        if compare(p1, p2) and  compare(p2, p3) and not compare(p1, p3): return False
        # # antisymmetry 
        if compare(p1, p2) and compare(p2, p1) and not equals(p1, p2): return False
        # # total order 
        if not (compare(p1, p2) or compare(p2, p1)): return False




    return True


formulas = list(filter(good_order, cnf_formulas))
print(formulas)

