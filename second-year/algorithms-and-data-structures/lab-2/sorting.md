# Task: SOR (Cellular Sorting)

Laboratory exercise for ASD, lab 2. Available memory: 64 MB. Deadline: 06.11.2022, 23:59:59.

Cellular sorting is a very interesting algorithm with relatively high time complexity for sorting. This algorithm works step-by-step, meaning it performs a certain step (sequence of operations) on a given sequence until the sequence becomes sorted in non-decreasing order.

The sorting step is as follows: we analyze the sequence from left to right and build the result sequence of the step on the side. Initially, we put the first element of the current sequence into the result sequence, and then each subsequent element is placed at the beginning of the auxiliary sequence if the previous element of the original sequence was greater than it, and at the end if the previous element was smaller. For example, in one step of the algorithm, the sequence: 5, 6, 2, 1, 4, 3 results in the following auxiliary sequences:

- 5,
- 5, 6,
- 2, 5, 6,
- 1, 2, 5, 6,
- 1, 2, 5, 6, 4,
- 3, 1, 2, 5, 6, 4,

and the last of them is the result of this step of the algorithm.

Your task is to "unsort" a given sequence, that is, to determine how many different sequences transform into this sequence in one step of the algorithm.

## Input

The first line of input contains a single integer n (1 ≤ n ≤ 1000). The second line contains a sequence of n pairwise distinct integers from the set {1, ..., n}, representing the sequence to be unsorted.

## Output

You should print the remainder of the division by 10^9 of the number of different sequences that transform into the given sequence in one step of cellular sorting.

## Example

For the input:

```plaintext
4
1 2 3 4
```

the correct output is:

```plaintext
8
```

whereas for the input:

```plaintext
4
4 3 2 1
```

the correct output is:

```plaintext
0
```

### Explanation of the example:

In one step of the sorting algorithm, the sequence 1, 2, 3, 4 can be obtained from the sequences:

- 1, 2, 3, 4,
- 4, 3, 2, 1,
- 2, 1, 3, 4,
- 3, 2, 1, 4,
- 2, 3, 1, 4,
- 2, 3, 4, 1,
- 3, 4, 2, 1,
- 3, 2, 4, 1,

whereas no other sequence transforms into the sequence 4, 3, 2, 1.
