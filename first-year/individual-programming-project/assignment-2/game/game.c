#include "game.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// finds the root of the nodes connected to the given one
//used to quickly check if two cells are connected
static index_t find(game_t* g, index_t n) {
  if (g->board[n].parent == n)
    return n;
  g->board[n].parent = find(g, g->board[n].parent);
  return g->board[n].parent;
}

//connects two areas of nodes
static void unite(game_t* g, index_t m, index_t n) {
  g->board[find(g, n)].parent = find(g, m);
}

//checks if the game is not null
static bool game_exists(const game_t* g) {
  return g != NULL;
}

//returns a reference to a cell with a given indices
static node_t* get_cell(const game_t* g, axis_t row, axis_t col) {
  if (game_exists(g))
    return &g->board[(index_t)row * (index_t)g->n_col + col];
  return NULL;
}

//checks if a cell is connected to any cell
static bool has_neighbour(game_t const* g, axis_t row, axis_t col,
                          player_t player) {
  if (game_exists(g))
    return (row > 0 && get_cell(g, row - 1, col)->val == player) ||
           (row < g->n_row - 1 && get_cell(g, row + 1, col)->val == player) ||
           (col > 0 && get_cell(g, row, col - 1)->val == player) ||
           (col < g->n_col - 1 && get_cell(g, row, col + 1)->val == player);
  return false;
}

//returns an index to a cell(any) that the given cell is connected to
static index_t get_neighbour(game_t const* g, axis_t row, axis_t col,
                             player_t player) {
  if (game_exists(g)) {
    if (row > 0 && get_cell(g, row - 1, col)->val == player)
      return (index_t)(row - 1) * (index_t)g->n_col + col;
    if (row < g->n_row - 1 && get_cell(g, row + 1, col)->val == player)
      return (index_t)(row + 1) * (index_t)g->n_col + col;
    if (col > 0 && get_cell(g, row, col - 1)->val == player)
      return (index_t)row * (index_t)g->n_col + col - 1;
    if (col < g->n_col - 1 && get_cell(g, row, col + 1)->val == player)
      return (index_t)row * (index_t)g->n_col + col + 1;
    return (index_t)g->n_col * (index_t)g->n_row;
  }
  return 0;
}

//used for sorting
static int compare(const void* a, const void* b) {
  const index_t* pa = (const index_t*)a;
  const index_t* pb = (const index_t*)b;
  return (*pa > *pb) - (*pa < *pb);
}

//returns n - 1 where n is a number of disjoint areas that are neighbours to a
// given empty cell. connects those areas as well.
static cells_t count_connect_disconnected_neighbours(game_t* g, axis_t row,
                                                     axis_t col,
                                                     player_t player) {
  if (game_exists(g)) {
    //find a "main" neighbour
    index_t f = get_neighbour(g, row, col, player);
    //fit parents of all neighbours in an array
    index_t arr[] = {
        (row > 0 && get_cell(g, row - 1, col)->val == player)
            ? find(g, (index_t)(row - 1) * (index_t)g->n_col + col)
            : (index_t)g->n_col * (index_t)g->n_row,
        (row < g->n_row - 1 && get_cell(g, row + 1, col)->val == player)
            ? find(g, (index_t)(row + 1) * (index_t)g->n_col + col)
            : (index_t)g->n_col * (index_t)g->n_row,
        (col > 0 && get_cell(g, row, col - 1)->val == player)
            ? find(g, (index_t)row * (index_t)g->n_col + col - 1)
            : (index_t)g->n_col * (index_t)g->n_row,
        (col < g->n_row - 1 && get_cell(g, row, col + 1)->val == player)
            ? find(g, (index_t)row * (index_t)g->n_col + col + 1)
            : (index_t)g->n_col * (index_t)g->n_row,
    };
    //sort the array
    qsort(arr, 4, sizeof(index_t), compare);
    cells_t count = 0;
    //count neighbours with the same parent
    for (uint8_t i = 1; i < 4; i++) {
      count += (arr[i - 1] != arr[i] &&
                arr[i] != (index_t)g->n_col * (index_t)g->n_row);
    }
    //connect all neighbours to the main neighbour
    if (row > 0 && get_cell(g, row - 1, col)->val == player)
      unite(g, f, (index_t)(row - 1) * (index_t)g->n_col + col);
    if (row < g->n_row - 1 && get_cell(g, row + 1, col)->val == player)
      unite(g, f, (index_t)(row + 1) * (index_t)g->n_col + col);
    if (col > 0 && get_cell(g, row, col - 1)->val == player)
      unite(g, f, (index_t)row * (index_t)g->n_col + col - 1);
    if (col < g->n_row - 1 && get_cell(g, row, col + 1)->val == player)
      unite(g, f, (index_t)row * (index_t)g->n_col + col + 1);
    //return the change in the number of disconnected neighbours
    return count;
  }
  return 0;
}

//counts cells where a player that has a maximum number of areas can place his mark
static cells_t count_free_next_to(game_t const* g, player_t player) {
  if (game_exists(g)) {
    cells_t sum = 0;
    for (axis_t row = 0; row < g->n_row; row++) {
      for (axis_t col = 0; col < g->n_col; col++) {
        sum += (get_cell(g, row, col)->val == 0 &&
                has_neighbour(g, row, col, player));
      }
    }
    return sum;
  }
  return 0;
}

game_t* game_new(axis_t width, axis_t height, player_t players,
                 uint32_t areas) {
  if (width == 0 || height == 0 || players == 0 || areas == 0 ||
      players > MAX_PLAYER_COUNT) {
    errno = EINVAL;
    return NULL;
  }
  game_t* g = malloc(sizeof(game_t));
  if (!game_exists(g)) {
    errno = ENOMEM;
    return NULL;
  }
  g->n_col = width;
  g->n_row = height;
  g->n_players = players;
  g->max_areas = areas;
  g->n_free_cells = (cells_t)width * (cells_t)height;
  g->board = (node_t*)calloc((cells_t)width * (cells_t)height, sizeof(node_t));
  if (g->board == NULL) {
    free(g);
    errno = ENOMEM;
    return NULL;
  }
  g->cells_player = (cells_t*)calloc(players + 1, sizeof(cells_t));
  g->areas_player = (cells_t*)calloc(players + 1, sizeof(cells_t));
  if (g->cells_player == NULL || g->areas_player == NULL) {
    game_delete(g);
    errno = ENOMEM;
    return NULL;
  }
  return g;
}

void game_delete(game_t* g) {
  if (game_exists(g)) {
    if (g->board != NULL)
      free(g->board);
    if (g->cells_player != NULL)
      free(g->cells_player);
    if (g->areas_player != NULL)
      free(g->areas_player);
    free(g);
  }
}

bool game_move(game_t* g, player_t player, axis_t x, axis_t y) {
  if (game_exists(g) && x < g->n_col && y < g->n_row &&
      get_cell(g, y, x)->val == 0 && player <= g->n_players && player >= 1) {
    if (g->areas_player[player] < g->max_areas ||
        has_neighbour(g, y, x, player)) {
      if (has_neighbour(g, y, x, player)) {
        g->areas_player[player] -=
            count_connect_disconnected_neighbours(g, y, x, player);
        index_t f = find(g, get_neighbour(g, y, x, player));
        node_t* cell = get_cell(g, y, x);
        cell->val = player;
        cell->parent = f;

      } else {
        g->areas_player[player]++;
        node_t* cell = get_cell(g, y, x);
        cell->val = player;
        cell->parent = (index_t)y * (index_t)g->n_col + x;
      }
      g->cells_player[player]++;
      g->n_free_cells--;
      return true;
    }
    return false;
  }
  return false;
}

cells_t game_busy_fields(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1)
    return g->cells_player[player];
  return 0;
}

cells_t game_free_fields(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1) {
    if (g->areas_player[player] < g->max_areas)
      return g->n_free_cells;
    return count_free_next_to(g, player);
  }
  return 0;
}

axis_t game_board_width(game_t const* g) {
  if (game_exists(g))
    return g->n_col;
  return 0;
}

axis_t game_board_height(game_t const* g) {
  if (game_exists(g))
    return g->n_row;
  return 0;
}

player_t game_players(game_t const* g) {
  if (game_exists(g))
    return g->n_players;
  return 0;
}

char game_player(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1)
    return '0' + player;
  return '.';
}

char* game_board(game_t const* g) {
  if (game_exists(g)) {
    char* field =
        (char*)malloc(((index_t)g->n_col + 1) * (index_t)g->n_row + 1);
    if (field == NULL) {
      errno = ENOMEM;
      return NULL;
    }

    for (axis_t row = g->n_row; row > 0; row--) {
      for (axis_t col = 0; col < g->n_col; col++) {
        field[(g->n_row - row) * (g->n_col + 1) + col] =
            game_player(g, g->board[(row - 1) * g->n_col + col].val);
      }
      field[(g->n_row - row) * (g->n_col + 1) + g->n_col] = '\n';
    }
    field[((index_t)g->n_col + 1) * (index_t)g->n_row] = '\0';
    return field;
  }
  return NULL;
}
