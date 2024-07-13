# Task: KAP (Captain)

Captain Bajtazar sails the waters of the Bajtoc Sea with his indispensable first officer, Bajtek. There are n islands in the sea, numbered from 1 to n. Captain's ship is docked at island number 1. During the voyage, the captain plans to sail to island number n.

During the journey, the ship always moves in one of the four cardinal directions: north, south, east, or west. At any given moment, either the captain or the first officer is at the helm. Each time the ship makes a 90-degree turn, they switch at the helm.

Along the way, the ship may stop at other islands. After each stop, the captain can decide whether he takes the helm first. In other words, on each leg of the journey from one island to another, one of the sailors takes the helm when the ship sails north or south, and the other sailor steers when sailing east or west. Specifically, if a leg of the journey leads exactly in one of the four cardinal directions, only one sailor steers on that leg.

The captain is now considering how to plan the route for the next voyage and the division of work so that he spends as little time at the helm as possible. At the same time, the captain does not care how long the designated route will be. We assume that the ship sails at a constant speed of one unit per hour.

## Input

The first line of input contains a single integer n (2 ≤ n ≤ 200,000), indicating the number of islands in the sea. For simplicity, we map the Bajtoc Sea with a coordinate system, where the axes are parallel to the cardinal directions. Each island is represented as a single point. The next n lines contain descriptions of the islands: the i-th of these lines contains two integers x_i, y_i (0 ≤ x_i, y_i ≤ 1,000,000,000), representing the coordinates of the i-th island in the sea. Each island has different coordinates.

## Output

Your program should print one integer, indicating the minimum number of hours the captain will have to steer the ship on the route from island number 1 to island number n.

## Example

For the input:

```plaintext
5
2 2
1 1
4 5
7 1
6 7
```

the correct output is:

```plaintext
2
```

### Explanation of the example:

The captain can plan a route as marked in the picture. During the voyage from island 1 (coordinates (2, 2)) to island 4 (coordinates (7, 1)), the captain steers only for one hour when the ship sails south. During the second leg of the journey, the captain steers only when the ship moves east.
