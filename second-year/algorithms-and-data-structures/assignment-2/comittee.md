# Task: KOM (Strike Committee)

As a result of the management's decision at Bajtock Factory of Goods to stop heating to save costs, a general strike has begun. Since the operation of the factory is crucial for the economy of Bajtocja, the management agreed to mediation. The strikers need to select a strike committee from among themselves to represent them in the negotiations.

This is not an easy task. The factory employs n workers with ranks corresponding to their experience. A worker can feel represented by a committee member only if their ranks differ by at most k. Moreover, if two committee members differ in rank by less than l, it leads to conflicts between them because the member with a lower rank does not recognize the authority of the more experienced one. Help the strikers form a committee such that every factory worker feels represented and internal conflicts among its members are avoided.

## Input

The first line of input contains three integers n, k, and l (1 ≤ n ≤ 500,000, 0 ≤ k, l ≤ 10^9) indicating the number of factory workers and the parameters corresponding to the strikers' requirements for the committee. The next line contains n integers in the range [0, 10^9], representing the ranks of the individual workers.

## Output

Your program should print two numbers separated by a space: the first is the size of the minimum committee that meets the strikers' requirements, and the second is the number of ways to form such a minimum committee (since this value can be large, we are interested in the result modulo 10^9 + 7). You can assume that the test data always allows for at least one committee that meets the given requirements.

## Example

For the input:

```plaintext
6 2 3
1 2 3 4 5 6
```

the correct output is:

```plaintext
2 6
```

### Explanation of the example:

In this example, it is not possible to form a one-person committee. However, a two-person committee can be composed of the pairs (1, 4), (1, 5), (1, 6), (2, 5), (2, 6), and (3, 6).
