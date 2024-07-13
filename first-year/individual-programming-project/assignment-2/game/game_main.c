#include <errno.h>
#include <ncurses.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "game.h"

#define FIELD_SPACE 2
#define KEY_CTRL_D 4

typedef struct {
  int row;
  int col;
} coords_t;

static struct {
  //offset from the beginning of the screen
  int first_row;
  int first_col;
} params;

/** @brief creates a new coords_t instance based on arguments provided
 *
 */
static coords_t new_coords(int row, int col) {
  coords_t coords;
  coords.row = row;
  coords.col = col;
  return coords;
}

/** @brief returns sum of two coords_t instances
 *
 */
static coords_t coords_sum(coords_t a, coords_t b) {
  a.row += b.row;
  a.col += b.col;
  return a;
}

/** @brief calculates position on the screen of a given point
 *
 */
static coords_t coords_to_display(coords_t a) {
  a.row += params.first_row;
  a.col *= FIELD_SPACE;
  a.col += params.first_col;
  return a;
}

/** @brief prints error info
 *
 */
static void print_error() {
  switch (errno) {
    case 0:
      break;
    case EINVAL:
      fprintf(stderr, "Got EINVAL error. Usage: ./game width height players areas.\n");
      break;

    case ENOMEM:
      fprintf(stderr, "Got ENOMEM error. Someone somewhere didn't get enough memory.\n");

    case ERANGE:
      fprintf(stderr, "Got ERANGE error. Probably an input argument is too large/small\n");

    default:
      fprintf(stderr, "Got error code: %d\n", errno);
      break;
  }
}

/** @brief input argument to uint
 *
 * @param arg       - program input argument(argv)
 * @param result    - where to place the result
 * @return          - 1 if successful 0 otherwise
 */
static bool argtoi(char* arg, uint32_t* result) {
  errno = 0;
  char* end = arg;
  uint64_t val = strtoull(arg, &end, 0);

  if (errno == ERANGE || *end != '\0' || val > UINT32_MAX)
    return 0;

  *result = (uint32_t)val;
  return 1;
}

/** @brief initialize ui
 *
 */
static void start_interface(void) {
  initscr();
  noecho();
  keypad(stdscr, true);
  cbreak();
}

/** @brief print game field on the screen
 *
 */
static void show_field(game_t* g) {
  char* field = game_board(g);

  if (field == NULL) {
    print_error();
    game_delete(g);
    exit(1);
  }

  for (axis_t row = 0; row < g->n_row; row++) {
    for (axis_t col = 0; col < g->n_col; col++) {
      coords_t d = coords_to_display(new_coords((int)row, (int)col));
      mvprintw(d.row, d.col, "%*c", FIELD_SPACE,
               field[row * (g->n_col + 1) + col]);
    }
  }

  free(field);
}

/** @brief print a message under the field
 *
 */
static void show_message(game_t const* g, char* msg) {
  coords_t d = coords_to_display(new_coords((int)g->n_row, 0));
  mvprintw(d.row + 1, d.col, "%s", msg);
}

/** @brief mark a given cell on the screen as of a given player
 *
 */
static void replace_cell(game_t* g, player_t player, coords_t p) {
  coords_t d = coords_to_display(p);
  mvprintw(d.row, d.col, "%*c", FIELD_SPACE, game_player(g, player));
}

/** @brief show a message under the field that a player has to move
 *
 */
static void print_current_player(game_t const* g, player_t p) {
  char pl = game_player(g, p);
  char m[] = "Player to make a move:  ";
  m[strlen(m) - 1] = pl;
  show_message(g, m);
}

/** @brief find the next player that can move and print the following
 * message
 *
 */
static bool next_player(game_t* g, player_t* p) {
  player_t temp = *p;

  do {
    temp %= g->n_players;
    temp++;
    if (game_free_fields(g, temp) != 0) {
      *p = temp;
      print_current_player(g, *p);
      return true;
    }
  } while (temp != *p);
  return false;
}

/** @brief check if a given position of the cursor is valid
 *
 */
static bool valid_postition(coords_t m, game_t* g) {
  return m.col >= 0 && m.row >= 0 && (axis_t)m.col < g->n_col &&
         (axis_t)m.row < g->n_row;
}

/** @brief move the cursor to a given position
 *
 */
static void move_cursor(coords_t p) {
  coords_t d = coords_to_display(p);
  move(d.row, d.col + 1);
}

/** @brief decide if the cursor has to be moved
 *
 * @param c     - input char
 * @return      - motion of the cursor
 */
static coords_t get_move(int c) {
  switch (c) {
    case KEY_LEFT:
    case KEY_SLEFT:
      return new_coords(0, -1);

    case KEY_RIGHT:
    case KEY_SRIGHT:
      return new_coords(0, 1);

    case KEY_DOWN:
    case KEY_SF:
      return new_coords(1, 0);

    case KEY_UP:
    case KEY_SR:
      return new_coords(-1, 0);

    default:
      return new_coords(0, 0);
  }
}

/** @brief play the game until an endpoint is reached
 *
 */
static void play_game(game_t* g) {
  player_t current_player = 1;
  coords_t p = new_coords(0, 0);
  int c;
  bool continue_play = true;

  print_current_player(g, current_player);
  move_cursor(p);
  refresh();

  while (continue_play && (c = getch()) != KEY_CTRL_D) {
    switch (c) {
      case 'C':
      case 'c':
        next_player(g, &current_player);
        break;

      case ' ':
        if (game_move(g, current_player, p.col, g->n_row - 1 - p.row)) {
          replace_cell(g, current_player, p);
          continue_play = next_player(g, &current_player);
        } else {
          show_message(g, "Can't make such move!    ");
        }
        break;

      default: {
        coords_t m = coords_sum(get_move(c), p);
        if (valid_postition(m, g)) {
          p = m;
          print_current_player(g, current_player);
        }
        break;
      }
    }

    move_cursor(p);
    refresh();
  }
}

static void print_results(game_t* g) {
  char* b = game_board(g);
  cells_t maximum = 0;
  uint32_t count = 0;

  if (b == NULL) {
    print_error();
    game_delete(g);
    exit(1);
  }

  printf("%s\n", b);
  free(b);

  for (player_t p = 1; p <= g->n_players; p++) {
    printf("Player %c has %lu cells\n", game_player(g, p), g->cells_player[p]);
    if (g->cells_player[p] > maximum) {
      maximum = g->cells_player[p];
      count = 1;
    } else if (g->cells_player[p] == maximum) {
      count++;
    }
  }

  if (count == 1) {
    for (player_t p = 1; p <= g->n_players; p++) {
      if (g->cells_player[p] == maximum) {
        printf("And the winner is... %c! Congragulations!\n",
               game_player(g, p));
      }
    }
  } else {
    printf("And the winners are... ");
    for (player_t p = 1; p <= g->n_players; p++) {
      if (g->cells_player[p] == maximum) {
        count--;
        printf("%c", game_player(g, p));
      }
      if (count != 0) {
        printf(", ");
      }
    }

    printf("! Congragulations!\n");
  }
}

int main(int argc, char** argv) {
  game_t* game;
  axis_t width;
  axis_t height;
  player_t players;
  uint32_t areas;
  errno = 0;

  if (argc != 5 || !argtoi(argv[1], &width) || !argtoi(argv[2], &height) ||
      !argtoi(argv[3], &players) || !argtoi(argv[4], &areas)) {
    errno = EINVAL;
    print_error();
    return 1;
  }

  if ((game = game_new(width, height, players, areas)) == NULL) {
    fprintf(stderr, "The game has not been created, please see the error below.\n");
    print_error();
    return 1;
  }

  start_interface();

  if (game->n_col > INT_MAX || game->n_row > INT_MAX || LINES < (int)game->n_row ||
      COLS < (int)FIELD_SPACE * (int)game->n_col) {
    endwin();
    fprintf(stderr, "Terminal window is too small or the board is too large.\n");
    game_delete(game);
    return 1;
  }

  params.first_row = (LINES - (int)game->n_row) / 2;
  params.first_col = (COLS - FIELD_SPACE * (int)game->n_col) / 2;

  show_field(game);
  play_game(game);
  endwin();
  print_results(game);
  game_delete(game);
  return 0;
}
