# Task: CHO (Christmas Tree)

The holidays are coming! Bajtazar is starting to plan the decoration of the Christmas tree. He has already developed an initial scheme for placing the baubles but will still be changing it, replacing some baubles with others. His refined aesthetic sense makes him pay particular attention to those branches of the tree where all — or almost all — baubles are of the same color. Write a program to help him in planning.

## Input

The first line of standard input contains two integers: n (2 ≤ n ≤ 200,000) and q (1 ≤ q ≤ 200,000). The number n is the number of nodes in the tree representing the Christmas tree, numbered from 1 to n, with node 1 being the root of the tree. The number q is the number of commands to execute.

The second line contains n - 1 numbers a*1, ..., a*{n-1}, where 1 ≤ a_i ≤ n. The number a_i is the parent node number of node i + 1.
The third line contains n numbers k_1, ..., k_n, where 1 ≤ k_i ≤ 1,000,000,000. The number k_i is the color of the bauble placed in node i.
Each of the next q lines contains one of two commands:

- `z v x` (1 ≤ v ≤ n, 1 ≤ x ≤ 1,000,000,000): change the bauble placed in node v to a bauble of color x.
- `? v` (1 ≤ v ≤ n): check whether the subtree of node v is almost monochromatic, i.e., whether in the nodes of this subtree all baubles except at most one are of the same color.

## Output

For each `? v` command in the input, the output should contain one line with the word TAK if the subtree of v is almost monochromatic, and NIE otherwise.

## Example

For the input:

```plaintext
5 5
1 2 1 3
3 1 2 5 4
? 2
z 5 1
? 2
z 2 2
? 2
```

the correct output is:

```plaintext
NIE
TAK
TAK
```
