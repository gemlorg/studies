# Bazaar Problem

Little Bajtek is spending his holidays with his grandmother Bajtula. Every morning, his grandmother goes to the bazaar to buy certain products. The boy quickly noticed an interesting pattern: each day his grandmother spends an amount expressed as an odd integer. Bajtek soon determined that this observed pattern is a characteristic feature of all Bajtok grandmothers.

Every day, grandmother Bajtula buys at most one of each of the n products available at the bazaar. In her prudence, she does not want to take too much money for shopping. One day, she asked Bajtek for advice on how much money she should take if she wants to buy exactly k products at the bazaar that day. Unfortunately, Bajtek does not know which products his grandmother intends to buy, so the amount of money taken must be enough for any k products (so that their total cost is odd). This situation repeated several times. Bajtek decided to approach the matter methodically and write a program that, given the prices of all products available at the bazaar, will answer his grandmother's questions.

## Input

The first line of input contains a single integer n (1 ≤ n ≤ 1,000,000) indicating the number of products available at the bazaar. The second line contains n integers in the range [1, 10^9], indicating the prices of individual products. These numbers are given in non-decreasing order.

The third line contains a single integer m (1 ≤ m ≤ 1,000,000) indicating the number of days Bajtek will still spend with his grandmother. Each of the next m lines contains a single integer k_i (1 ≤ k_i ≤ n), indicating the number of products that grandmother intends to buy on that day.

## Output

Your program should print m lines. In the i-th line (for i = 1, ..., m), there should be a single integer indicating the maximum odd price of k_i products. If it is not possible to choose k_i products whose total price would be odd, the i-th line should contain the number -1.

## Example

For the input:

```plaintext
4
1 2 3 4
3
1
2
3
```

the correct output is:

```plaintext
7
9
-1

```
