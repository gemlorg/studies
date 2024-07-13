# Assignment description

This year's task is to implement a game. The game is played on a rectangular board consisting of identical square fields. Adjacent fields on the board are those that touch at their sides. Fields that only touch at the corners are not considered adjacent. Fields form an area if each of them can be reached from another by only passing through adjacent fields. A single field is also considered an area. One or more players can play the game.

At the start of the game, the board is empty. Players take turns rolling a traditional six-sided die. Each player occupies no more fields, placing their pawn on them, than the number of dots rolled on the die. Players can occupy any unoccupied fields, following the rule that the set of fields occupied by the same player can never consist of more than a specified maximum number of areas, a parameter of the game. A player who cannot make a move according to the above rules is out of the game. The game ends when no player can make a move. The player who occupies the largest number of fields wins.

As the first part of the task, you need to implement the "engine" module of the game in C. The interface description of the module is found in the `game.h` file in the format of comments for the doxygen program. An example usage can be found in the `game_example.c` file.

As a solution, you should provide:

- `game.h` file with the definition of the game engine interface,
- `game.c` file with the implementation of the game engine,
- possibly other `.h` and `.c` files with implementation,
- `game_example.c` file containing the `main` function,
- `makefile` that by default creates an executable file named `game`.

It is recommended to compile using the `gcc` command with the following parameters:

```sh
-Wall -Wextra -Wno-implicit-fallthrough -std=c17 -O2

```
