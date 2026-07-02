-# Advanced Architecture — Midterm (Part A)

**Prof. A. Massini**
**April 14, 2026**

Student's Name: **\*\***\*\*\*\***\*\***\_**\*\***\*\*\*\***\*\***

Matricola number: **\*\***\*\*\*\***\*\***\_**\*\***\*\*\*\***\*\***

| Exercise   | Points |
| ---------- | ------ |
| Exercise 1 | 6      |
| Exercise 2 | 6      |
| Exercise 3 | 5      |
| Exercise 4 | 3      |
| Exercise 5 | 6      |
| Exercise 6 | 6      |
| **Total**  | **32** |

---

## Exercise 1 (6 points) — Number representation

a) Determine the value of _n_ for a residue number system with a three module set based on powers of 2 defined as S = {2ⁿ⁺¹+1; 2ⁿ⁺¹−1; 2ⁿ}, such that the natural number range [0, 16299] is representable.

The representable range for such system is 0...lcm(2ⁿ⁺¹+1; 2ⁿ⁺¹−1; 2ⁿ)-1 (note that all of those are relatively prime, so so lcm is just multiplication); so we simply need to find the right n, so that all numbers from the required range are represented. for n=4, we get 2\**5 = 32; 31*32\*33 > 30^3 = 27000, so the range is representable.

b) Represent A = 98 and B = 26 using S. Then compute the product P = A × B using the residue number representation, and verify the correctness of the representation of the result P.
A = (98 mod 31, 98 mod 32, 98 mod 33) = (5, 2, 32)
B = (26, 26, 26)
P = ( 5 _-5 mod 31, 2_ -6 mod 32, -1 \* -7 mod 33) = (6, 20, 7)
**verification skipped for time preserving purposes, but assume ive calculated the full result and the modules**

c) Represent P in the mixed radix representation associated with S.
i actually dont remember how to do this, perhaps need to represent with chinese residue theorem

---

## Exercise 2 (6 points) — Number representation

a) Represent X = −65 using the RB (Redundant Binary) representation.
-1000000-1

b) Then represent X using a redundant representation with radix r = 7 and the digit set [−5, 5] using 3 digits, providing different valid representations. Also specify how many values can be represented, the redundancy index, and the redundancy percentage.
X = (-1 , -2, -2 ) = (-1, -3, 5)

since alpha=5, beta=5; redundancy index is 5 + 5 - 7 + 1 = 4, hence 4 different representations for X. Redundancy percentage is 4/7 \* 100 %

---

## Exercise 3 (5 points) — Circuit area and time

a) Consider the moduli set T = {7, 11, 13, 16} for a residue number system. Specify which ripple-carry adder modules should be used to design an adder for numbers represented using T, assuming radix-2 representation for each module and neglecting the logic required to ensure that the result lies within the correct range. Then compute the circuit area and the time required to perform 200 additions.
we can perform addition independently on each residue number, modulo that residue number. To implement addition per-modulo, we need one HA block and log*2(modulo) -1 FA blocks, so 1 HA + 2 FA for 7; 1 HA + 3FA for 11; 1HA + 3FA for 13; 1HA + 3FA for 16. which gives us an area of 3* 3 + 11 _7; delay of 1_ T*gate + 2* 3 \_T_gate = 7 \_T_gate ; so for 200 additions we require 1400 \* T_gate time.

b) Now consider a pipelined adder sized to operate over the range defined by the moduli set T in point a). Compute the area of the circuit and the time required to perform 200 additions.
I actually dont know how a pipelined adder looks like

c) Determine the number of additions for which the pipelined adder becomes more convenient, if any.
idk

---

## Exercise 4 (3 points) — Gustafson-Barsis' law

a) Briefly explain the difference between the Gustafson-Barsis' law and the Amdahl's law.

Amahl's law asks how much speed up will we get by using more parallel machines for the same fixed problem, while G-B's law asks how much more work we will be able to do in the same amount of time when using parallel machines.
They are given by formulas, respectively

1/ ((1 - f) + f / n) and (1 - f) + f \* N

b) The analysis of a program has shown a speedup of 8.5 when running on 24 cores. What is the serial fraction according to Gustafson-Barsis' law? And according to Amdahl's law?

1 - f + f \* 24 = 8.5 => 23 f = 75/10 => f = 75/230;

1/ (1 - f + f / 24) = 8.5 => 10/85 = 1 - 23/24 f => f = 24 / 23 \* 75/85

c) Considering the serial fraction obtained in point b), compute the speedup when using 32 cores according to Gustafson-Barsis' law and Amdahl's law.

**skip**

---

## Exercise 5 (6 points) — Amdahl's law

The following measurements are recorded for different instruction classes in an instruction set running a given set of benchmark programs:

| Instruction Type     | Instruction Count (millions) | Cycles Per Instruction |
| -------------------- | ---------------------------- | ---------------------- |
| Arithmetic and logic | 10                           | 6.8                    |
| Load and store       | 14                           | 4.2                    |
| Branch               | 8                            | 7.6                    |
| Others               | 12                           | 5.0                    |

Assume that "Load and store" instructions can be improved, achieving a speedup factor of 9, and that "Branch" instructions can be improved, achieving a speedup of 15.

a) Compute the speedup obtained by introducing only one enhancement and both enhancements, using Amdahl's law, and identify which alternative is more cost-effective.

b) Then compute the new CPI (Cycle Per Instruction) value for the two cases, using the specified speedup.

explain

---

## Exercise 6 (6 points) — Performance equation

Suppose we have the following measurements, where we consider the set of Arithmetic and Logic instructions (A&L), the subset of Arithmetic instructions (AR) and the subset of integer Multiplications and Divisions (MD):

- Frequency of A&L operations = 40%
- Average CPI of A&L operations = 6
- Frequency of AR = 20%
- CPI of AR = 4.5
- Frequency of MD = 10%
- CPI of MD = 8
- Average CPI of other (than A&L) instructions = 3.5

Assume that you have the following design alternatives:

i) reduce the average CPI of A&L instructions to 5;
ii) reduce the CPI of AR operations to 3.4;
iii) reduce the average CPI of MD operations to 6.4.

total CPI : 0.4 _6 + 0.6_ 3.5 = 2.4 + 1.8 + 0.3 = 4.5

if anl to 5: total cpi = 4.5 - 0.4 _1
if anr to 3.4: total cpi = 4.5 - 0.2_ (4.5 - 3.4) = 4.5 - 0.2 _1.1
if md to 6.4: total cpi = 4.5 - 0.1_ 1.6

Compare these three design alternatives using the processor performance equation and compute the speedup in each case.--

speedup is 4.5/4.1 ; 4.5 / 4.28 ; 4.5/4.34 in each case
