# Task: KIN (k-inversions)

Let \(a*1, \ldots, a_n\) be a permutation of numbers from 1 to n. A k-inversion in this permutation is a sequence of indices \(i_1, i_2, \ldots, i_k\) such that \(1 \leq i_1 < i_2 < \ldots < i_k \leq n\) and \(a*{i*1} > a*{i*2} > \ldots > a*{i_k}\). Your task is to determine the number of k-inversions in the given permutation.

## Input

The first line of input contains two integers n and k (1 ≤ n ≤ 20,000, 2 ≤ k ≤ 10). The second line contains a permutation of numbers {1, \ldots, n}.

## Output

Your program should print the remainder of the division by \(10^9\) of the number of k-inversions in the given permutation.

## Example

For the input:

```plaintext
4 3
4 3 1 2
```
