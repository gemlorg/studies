# Assignment description

## Objective

The second part of the task is to implement a game interface that allows gameplay and utilizes the engine module implemented in the first part. Additionally, in the second part of the task, errors from the first part of the task should be corrected.

## Running the Program

The program is executed in a Linux console with the command:

```sh
./game width height players areas
```

where the program parameters are the parameters for calling the `game_new` function. The program should thoroughly check the validity of the provided parameters.

## Exiting the Program

Generally, the program should exit with code 0, unless a critical error occurs – in which case the program should output an appropriate message to the standard diagnostic output and exit with code 1. Before ending, the program should explicitly free all allocated memory, specifically calling the `game_delete` function.

## Interactive Text Mode

The game program operates in text mode in the Linux console. The program displays the board, with a prompt line below encouraging the player to make a move. The program prompts each player to make a move, skipping players for whom the `game_free_fields` function returned 0. Moves are made by moving the cursor to the selected field using the arrow keys, then pressing the spacebar to place a pawn on the selected field. A player can forfeit their move by pressing the 'C' key. The game ends when no player can make a move or after pressing the Ctrl-D key combination once. The program then outputs the final game board along with a summary of how many fields each player occupied.

Pressing letter keys should be recognized independently of the Shift key and the CapsLock state. The program should prevent users from performing erroneous actions. The program should exit, displaying an appropriate error message if the board does not fit within the terminal window.

## Requirements

The solution for the second part of the task should use its own or independently modified solution from the first part. It is permissible to expand the game engine module with necessary functions, but not to modify the behavior of functions defined in the first part of the task.

### Deliverables:

- `*.c` and `*.h` files with the game implementation
- Makefile script compiling the program

It is recommended to compile using the command:

```sh
gcc -Wall -Wextra -Wno-implicit-fallthrough -std=c17 -O2
```

The solution should compile and run in the student lab and on the students' machine.

The `make clean` command should remove all files created by the `make` command.

Contents of the `game_example.c` and `game_test_1.c` files will not be evaluated – you can place your own tests in them.

**Note 1:** The main function of the program must be in the `game_main.c` file.

**Note 2:** The compilation should result in an executable file named `game`.
