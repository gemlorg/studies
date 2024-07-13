# Task: MEC (Matches)

Laboratory exercise for ASD, lab 3. Available memory: 128 MB. Deadline: 13.11.2022, 23:59:59.

In soccer training, there are n players (n is an even number). In each match, all players play, with n/2 in each team. The coach decided to arrange the teams in such a way that every pair of players has a chance to play against each other in some match (i.e., at least once play on opposing teams).

The coach has already proposed the lineups for the upcoming m matches. Help him determine whether he has achieved his goal.

## Input

The first line of input contains two integers n and m (4 ≤ n ≤ 40,000, 1 ≤ m ≤ 50), indicating the number of players and the number of scheduled matches. Players are numbered from 1 to n.

Each of the next m lines contains n pairwise distinct integers from the range 1 to n describing the team lineups for each match. The first n/2 numbers in each line are the player numbers playing in the first team, and the second n/2 numbers are the player numbers playing in the second team.

## Output

Your program should print one word: TAK (YES) or NIE (NO), depending on whether each pair of players has played against each other at least once, or not.

## Example

For the input:

```plaintext
6 3
4 6 1 3 5 2
1 4 5 2 3 6
1 2 6 4 5 3
```

the correct output is:

```plaintext
TAK
```

whereas for the input:

```plaintext
6 3
4 6 1 3 5 2
1 4 5 2 3 6
1 2 3 4 5 6
```

the correct output is:

```plaintext
NIE
```

### Explanation of the example:

In the first example, each pair of players plays on opposing teams in one match (e.g., players numbered 1 and 6), in two matches (e.g., players 1 and 2), or even in all three matches (e.g., players 1 and 3). In the second example, players numbered 2 and 3 always play on the same team.

## Hints

Focus on checking if there are two players who have never played on different teams. Apply the idea of labeling.
