# Task: PRJ (Projects)

Bajtazar has just been promoted to head of the IT department of a Very Important State Institution. His responsibilities include managing IT projects. The institution has prepared a list of potential projects that should be carried out. Unfortunately, the completion of some projects depends on the successful completion of others. Additionally, each project is characterized by the minimum number of programmers required to complete it.

Due to budget cuts, it is not possible to complete all the projects. The management has decided that only k projects will be carried out. Bajtazar was instructed to hire the minimum number of programmers necessary to complete at least k projects (where projects can be carried out sequentially, so that programmers are transferred from one project to another).

Write a program to help Bajtazar determine the minimum number of programmers that need to be hired.

## Input

The first line of standard input contains three integers n, m, and k (1 ≤ n ≤ 100,000, 0 ≤ m ≤ 500,000, 0 ≤ k ≤ n), separated by single spaces, indicating the number of projects, the number of dependencies between projects, and the minimum number of projects that must be completed, respectively. The next n lines contain information about the number of programmers required to complete each project. In the (i+1)-th line, there is an integer p_i (1 ≤ p_i ≤ 100,000,000) indicating that to complete the i-th project, p_i programmers are required. The following m lines contain information about the dependencies between projects. Each of these lines contains two integers a and b (1 ≤ a, b ≤ n, a ≠ b) separated by a single space, indicating that to complete project a, project b must be completed first.

You can assume that the dependencies between projects do not form cycles.

## Output

The single line of standard output should contain the minimum number of programmers that need to be hired to complete k projects.

## Example

For the input:

```plaintext
5 3 3
10
500
150
200
100
1 2
1 3
4 5
```

the correct output is:

```plaintext
200
```
