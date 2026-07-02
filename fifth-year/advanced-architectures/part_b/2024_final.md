## Exercise 1 (7 points)

Consider the sparse matrix here below, whose pattern is shown on the right.

a) Explain which arrays you need for the following representations, specifying their role and elements they contain.
b) Specify how many bytes the arrays occupy in memory.
c) Explain how arrays change after the deleting the elements m₂,₆ and m₁₀,₈ and what the new memory occupation is corresponds to.
d) Explain how arrays change after inserting the elements m₉,₁₀=9,35 and m₂₈,₁₀=27,81 and what the new memory occupation corresponds to.

|        | 1    | 2    | 3    | 4    | 5    | 6    | 7    | 8    | 9    | 10   | 11   | 12   |
| ------ | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| **1**  | 16.9 | 19.1 | 0    | 12.8 | 1.25 | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| **2**  | 19.1 | 25.5 | 13.2 | 21.9 | 0    | 25.5 | 0    | 0    | 0    | 0    | 0    | 0    |
| **3**  | 9.9  | 13.2 | 9.56 | 0    | 9.5  | 1.13 | 13.9 | 0    | 0    | 0    | 0    | 0    |
| **4**  | 0    | 0    | 0    | 18.4 | 12.9 | 8.2  | 4.5  | 0    | 2.7  | 0    | 0    | 0    |
| **5**  | 0    | 0    | 0    | 12.9 | 1.1  | 6.1  | 0    | 1.2  | 3.9  | 0    | 0    | 0    |
| **6**  | 0    | 0    | 0    | 0    | 6.1  | 4.6  | 0    | 2.7  | 3.9  | 0    | 0    | 0    |
| **7**  | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| **8**  | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| **9**  | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 0    |
| **10** | 0    | 0    | 0    | 0    | 0    | 0    | 0    | 16.3 | 0    | 12.8 | 0    | 9.5  |
| **11** | 0    | 0    | 0    | 0    | 0    | 0    | 12.5 | 24.9 | 0    | 16.3 | 22.9 | 0    |
| **12** | 0    | 0    | 0    | 0    | 0    | 0    | 18.4 | 22.5 | 25.5 | 17.7 | 25.5 | 13.9 |

_(Pattern plot: nz = 42)_

### Ellpack-Itpack

a) requires two rectangular arrays, each of dimensions n x Rnz where Rnz is max number of nz elements in a row, and n is number of rows. ; call these arrays VAL and IDX
VAL is real and each row stores non-zero elements of the original matrix, in order. IDX is integer and stores column indexes of corresponding VAL values in same positions.

b ) n = 12; Rnz = 6; int takes 4 bytes and real 8, so total is (8 + 4) _12_ 6

c) memory occupation doesnt change because rnz isnt affected
d) same as in c) because Rnz isnt affected

### diag

a) let Dnz denote number of non-zero diagonals; we'd need two arrays: one call VAL, real, of size N x Dnz where N is number of rows/cols (diag only for square matrices); in this arr each column stores all values of some diagonal with non-zero elements present (unordered, usually). the second array we need, call IDX is of size Dnz specifying offsets to each diagonal from the main diagonal, integer values.
b) dnz = 10 so 10 _4 + 10_ 12 \* 8
c, d) nothing changes because we dont add nor remove diagonals with non-zeror elements with any of those actions

---

## Exercise 2 (6 points) – Interconnection Networks

a) Design a Clos network of size 225 × 225, using modules having 20 inputs in the first and middle stages (the third stage is symmetrical to the first for the number of inputs and outputs). Specify the size and the number of switches for each stage. Consider both cases, strictly non-blocking and rearrangeable network.

so it's 3 stages

n x m => r x r => m x n
1 1 1
. . .
. . .
. . .
r m r

we get n=20?
so for non-blocking we need m >= 2n-1 ; m>= n for rearrangable

so the total formula for cost is:
size: 20 x 2 x m + r x r = 40m + r\*\*2
link: 2 x r x 20 + 2 x r x m = 40r + 2rm

but idk how 225 fits into this lol? how do i answer the question

b) Compute the cost of the crossbar 225 × 225, the cost of the Clos networks strictly non-blocking and rearrangeable non-blocking designed in the previous point, and the cost of the Benes with 256 inputs. Which network is more advantageous?

---

## Exercise 3 (4 points) – Interconnection networks

Explain how an Extended Generalized Fat Tree is made and show the representation of the XGFT(3; 2, 4, 2; 2, 4, 1).

_(answer space)_

---

## Exercise 4 (5 points) – Quantum circuits

Consider the two-qubit transformations U shown below:

```
        ───────●───[V]───
U
        ──[Y]──⊕─────────
```

where

Y = [ 0 −i ]
[ i 0 ]

and

V = ½ [
1+i 1−i
1−i 1+i
]

a) Show what transformation U represents writing the associated 4×4 matrix.
Note: tensor product is linear with respect to the base field, so 1/2 moves to the front

U =
( ID \ox Y) CNOT ( V \ox ID)

=
1/2 \[
0 -i 0 0
i 0 0 0
0 0 0 -i
0 0 i 0
\]
\[
1 0 0 0
0 1 0 0
0 0 0 1
0 0 1 0
\]
\[
1+i 0 1-i 0
0 1+i 0 1-i
1-i 0 1+i 0
0 1-i 0 1+i
\]
... calculate product directly

b) Show if U is unitary.
Unitary means conjugate+transpose => inverse, calculate directly if the product means ID

c) Show how U acts on the system state |ψ₁ψ₂⟩, where |ψ₁⟩ = (√2/√3)|0⟩ − (√3/3)i|1⟩ and |ψ₂⟩ = (2/6)|0⟩ + (1/√3)i|1⟩.
calculate the tensor product
=> \[
2 root 2 / 6 root 3
i root 2 / 3
-i 2 root 3 / 18
1/3
\]

make sure its normalized,
multiply with the matrix Ux => calculate results

---

## Exercise 5 (5 points) – Interconnection Networks

a) Complete the scheme of the Baseline network of size N=8 and show if it can realize permutation

P = ( 01 23 45 67 )
( 54 12 07 36 )

showing the switch setting obtained using the self-routing algorithm.

b) Complete the scheme of the Butterfly-Reverse Butterfly network of size N=8 and show how it can realize the permutation P using the Looping algorithm. Show how the algorithm proceeds in the diagram below.

---

## Exercise 6 (4 points) – Quantum computing

Briefly explain what entanglement is and which gates you can use to realize it.

Exercise 1 (7 points) – Interconnection Networks
a) Design a Clos network of size 920 x 920, using modules having 18 inputs in the first and middle stages (whereas
the third stage is symmetrical to the first), specifying the size and the number of switches for each stage. Consider
both cases, strictly non-blocking and rearrangeable network.

r = 52 - minimal
m >= 2n-1 = 35 for non-blocking
m >= n = 18 for reroutable

b) Describe how a Benes network is built and define how big a Benes network should be to accommodate 920
inputs/outputs.

_long explanation_
it needs 2 \* binlog N - 1 stages; each with N/2 2 by 2 swiches

c) Compare the cost of the Clos network designed in previous point a), in both cases strictly non-blocking and
rearrangeable and the Benes network defined in point b).

non blocking clos: m ( 2nr + r\*_2 )= 35 (2 52 18 + 52 52 ) = 35 52 (36 + 52) = 35 52 88
routing clos: 18 52 88
benes: ( 2_ 10 - 1) _460_ 4 = 19 _460_ 4
