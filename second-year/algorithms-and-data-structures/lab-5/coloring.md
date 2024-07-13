# Task: MAL (Painting the Highway)

Professor Makary, wanting to help the government of Bajtocja, paints the highway for free. The highway is n kilometers long and is divided into kilometer segments numbered 1, ..., n. The professor has white paint at his disposal.

Initially, the entire highway is black. At night, if he suffers from insomnia, Professor Makary goes out to the highway with a bucket of paint and paints a certain segment of the highway. Unfortunately, sometimes holes appear in the highway, and during the day a roller comes to lay asphalt. The asphalted part of the road naturally becomes black again. The professor would like to have up-to-date information on how many kilometers of the highway are painted white. Help the professor in this responsible task.

## Input

The first line of input contains an integer n (1 ≤ n ≤ 1,000,000), indicating the length of the highway. The second line contains an integer m (1 ≤ m ≤ 1,000,000), indicating the total number of painting nights and roller days. Each of the next m lines contains two integers 1 ≤ a ≤ b ≤ n and a letter c. The numbers a and b are the endpoints of the painted segment, c describes the event. 'B' means the professor painted the highway, and 'C' means the roller was used on it.

## Output

After reading each line, your program should print the number of kilometers painted white.

## Example

For the input:

```plaintext
12
4
1 5 C
2 10 B
4 6 B
4 7 C
```

the correct output is:

```plaintext
0
9
9
5
```
