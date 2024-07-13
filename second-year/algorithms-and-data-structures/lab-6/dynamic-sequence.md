# Task: DYN (Dynamic Sequence)

Implement a data structure that performs basic operations on a sequence of numbers, allowing for the insertion of elements. The elements of the sequence are numbered starting from 0. Initially, the sequence is empty. The following operations are allowed:

- `insert(j, x, k)` – inserts k copies of element x just before the j-th position of the sequence, shifting elements at positions j and beyond by k positions to the right;
- `get(j)` – returns the value of the j-th element of the sequence.

You can assume that for the insert operation 0 ≤ j ≤ n, and for the get operation 0 ≤ j < n, where n is the length of the sequence at the time of the operation.

In this task, the input format is disguised to enforce online solutions. In your solution, you can use ready implementations of balanced binary trees from the Internet.

## Input

The first line contains an integer m (1 ≤ m ≤ 500,000), indicating the number of operations. Each of the following m lines contains a lowercase letter indicating the type of operation – `i` for insert, `g` for get – followed by numbers separated by spaces: j', x, and k for insert, j' for get. The parameter j in a given query is determined based on the read number j' according to the formula j = (j' + w) mod (n + 1) for the insert operation and j = (j' + w) mod n for the get operation, where w is the result of the last get operation or 0 if there has not been any get operation before, and n is the length of the sequence at the time of the operation. The numbers j' and x will be non-negative and not greater than 10^9, and the numbers k will be in the range from 1 to 1000.

## Output

For each get operation, your program should print one line containing the result of that operation. You can assume that there will always be at least one operation of this type.

## Example

For the input:

```plaintext
7
i 0 2 3
i 1 1 2
g 2
i 4 1 1
g 2
i 1 3 2
g 2
```

the correct output is:

```plaintext
2
2
3
```

The input translates to the following sequence of queries:

| Input Operation | Translated Operation | Resulting Sequence     | Result |
| --------------- | -------------------- | ---------------------- | ------ |
| i 0 2 3         | insert(0, 2, 3)      | 2, 2, 2                | –      |
| i 1 1 2         | insert(1, 1, 2)      | 2, 1, 1, 2, 2          | –      |
| g 2             | get(2)               | no change              | 2      |
| i 4 1 1         | insert(5, 1, 1)      | 2, 1, 1, 2, 2, 1       | –      |
| g 2             | get(3)               | no change              | 2      |
| i 1 3 2         | insert(3, 3, 2)      | 2, 1, 1, 3, 3, 2, 2, 1 | –      |
| g 2             | get(4)               | no change              | 3      |
