# Task: MAT (Matrix)

The Bajtock Printing Plant (BZP) received a large order for producing striped wallpapers, which are a hit in interior design this season. Each wallpaper consists of n vertical stripes of equal width. BZP is tasked with designing and printing the wallpapers. The client has predetermined the colors of some stripes on the wallpaper. For the remaining stripes, BZP has full discretion.

To print the wallpapers, BZP uses matrices that print a certain number of consecutive stripes on the wallpaper. The matrix has specific colors for each of the printed stripes and can be shorter than the entire wallpaper. If the matrix consists of k stripes, it is applied in all n−k+1 possible positions where its stripes overlap with the wallpaper stripes, each time printing all the matrix stripes. Thus, one stripe on the wallpaper can be printed multiple times. If a stripe is printed in different colors, its final color will be a mix of those colors.

The employees of BZP, regardless of their aesthetic sense, primarily want to design the shortest possible matrix that can print the entire wallpaper. They must remember that for the stripes specified by the client, they must use a pure color without mixing with other colors. In other words, each time the matrix is applied to cover such a stripe, the color of the stripe on the matrix must exactly match the one specified by the client.

## Input

The only line of input contains a string consisting of uppercase Latin letters and asterisks (\*), specifying the desired appearance of the wallpaper. The letters represent different stripe colors, while the asterisks represent stripes whose color was not specified by the client. The length of the string n is such that 1 ≤ n ≤ 1,000,000.

## Output

Your program should print a single integer k: the minimum length of the matrix that allows for printing the desired wallpaper.

## Example

For the input:

```plaintext
A*B*B*A
```

The correct output is:

```plaintext
6
```
