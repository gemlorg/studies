#include "game.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct node {
  //number of a player who has put his mark on the cell, 0 otherwise
  value_t val;
  // a cell connected to this one and considered to be more recent
  index_t parent;
} node_t;

typedef struct game {
  //game field
  node_t* board;  //[row][column]
  axis_t n_col;
  axis_t n_row;
  player_t n_players;
  cells_t n_free_cells;
  cells_t max_areas;
  //cells and areas taken per each player
  cells_t* cells_player;
  cells_t* areas_player;
} game_t;

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
  if (game_exists(g) && row < MAX_INPUT_NUMBER && col < MAX_INPUT_NUMBER)
    return &g->board[(index_t)row * (index_t)g->n_col + col];
  return NULL;
}

//checks if a cell is connected to any cell
static bool has_neighbour(game_t const* g, axis_t row, axis_t col,
                          player_t player) {
  if (game_exists(g) && row < MAX_INPUT_NUMBER && col < MAX_INPUT_NUMBER)
    return (row > 0 && get_cell(g, row - 1, col)->val == player) ||
           (row < g->n_row - 1 && get_cell(g, row + 1, col)->val == player) ||
           (col > 0 && get_cell(g, row, col - 1)->val == player) ||
           (col < g->n_row - 1 && get_cell(g, row, col + 1)->val == player);
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
    if (col < g->n_row - 1 && get_cell(g, row, col + 1)->val == player)
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

/** @brief Tworzy strukturę przechowującą stan gry.
 * Alokuje pamięć na nową strukturę przechowującą stan gry.
 * Inicjuje tę strukturę, tak aby reprezentowała początkowy
 * stan gry. Gdy nie udało się alokować pamięci, ustawia @p
 * errno na @p ENOMEM.
 * @param[in] width   – szerokość planszy, liczba dodatnia,
 * @param[in] height  – wysokość planszy, liczba dodatnia,
 * @param[in] players – liczba graczy, liczba dodatnia,
 * @param[in] areas   – maksymalna liczba obszarów, które
 * może zająć jeden gracz, liczba dodatnia.
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie
 * udało się alokować pamięci lub któryś z parametrów jest
 * niepoprawny.
 */
game_t* game_new(axis_t width, axis_t height, player_t players,
                 uint32_t areas) {
  if (width == 0 || height == 0 || players == 0 || areas == 0 ||
      players > MAX_PLAYER_COUNT || width > MAX_INPUT_NUMBER ||
      height > MAX_INPUT_NUMBER || areas > MAX_INPUT_NUMBER)
    return NULL;
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

/**  @brief Usuwa strukturę przechowującą stan gry.
 * Usuwa z pamięci strukturę wskazywaną przez @p g.
 * Nic nie robi, jeśli wskaźnik ten ma wartość NULL.
 * @param[in] g       – wskaźnik na usuwaną strukturę.
 */
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

/** @brief Wykonuje ruch.
 * Ustawia pionek gracza @p player na polu (@p x, @p y).
 * @param[in,out] g   – wskaźnik na strukturę przechowującą
 * stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia
 * niewiększa od wartości
 *                      @p players z funkcji @ref game_new,
 * @param[in] x       – numer kolumny, liczba nieujemna
 * mniejsza od wartości
 *                      @p width z funkcji @ref game_new,
 * @param[in] y       – numer wiersza, liczba nieujemna
 * mniejsza od wartości
 *                      @p height z funkcji @ref game_new.
 * @return Wartość @p true, jeśli ruch został wykonany, a @p
 * false, gdy ruch jest nielegalny, któryś z parametrów jest
 * niepoprawny lub wskaźnik @p g ma wartość NULL.
 */
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
        cell->parent = (index_t)y * (index_t)g->n_row + x;
      }
      g->cells_player[player]++;
      g->n_free_cells--;
      return true;
    }
    return false;
  }
  return false;
}

/** @brief Podaje liczbę pól zajętych przez gracza.
 * Podaje liczbę pól zajętych przez gracza @p player.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia
 * niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Liczba pól zajętych przez gracza lub zero,
 * jeśli któryś z parametrów jest niepoprawny lub wskaźnik
 * @p g ma wartość NULL.
 */
cells_t game_busy_fields(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1)
    return g->cells_player[player];
  return 0;
}

/** @brief Podaje liczbę pól, które jeszcze gracz może
 * zająć. Podaje liczbę wolnych pól, na których w danym
 * stanie gry gracz @p player może postawić swój pionek w
 * następnym ruchu.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia
 * niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Liczba pól, jakie jeszcze może zająć gracz lub
 * zero, jeśli któryś z parametrów jest niepoprawny lub
 * wskaźnik @p g ma wartość NULL.
 */
cells_t game_free_fields(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1) {
    if (g->areas_player[player] < g->max_areas)
      return g->n_free_cells;
    return count_free_next_to(g, player);
  }
  return 0;
}

/** Podaje szerokość planszy.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry.
 * @return Szerokość planszy lub zero, gdy wskaźnik @p g ma
 * wartość NULL.
 */
axis_t game_board_width(game_t const* g) {
  if (game_exists(g))
    return g->n_col;
  return 0;
}

/** Podaje wysokość planszy.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry.
 * @return Wysokość planszy lub zero, gdy wskaźnik @p g ma
 * wartość NULL.
 */
axis_t game_board_height(game_t const* g) {
  if (game_exists(g))
    return g->n_row;
  return 0;
}

/** Podaje liczbę graczy.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry.
 * @return Liczba graczy lub zero, gdy wskaźnik @p g ma
 * wartość NULL.
 */
player_t game_players(game_t const* g) {
  if (game_exists(g))
    return g->n_players;
  return 0;
}

/** Daje symbole wykorzystywane w funkcji @ref game_board.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia
 * niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Cyfra, litera lub inny jednoznakowy symbol
 * gracza. Symbol oznaczający puste pole, gdy numer gracza
 * jest niepoprawny lub wskaźnik @p g ma wartość NULL.
 */
char game_player(game_t const* g, player_t player) {
  if (game_exists(g) && player <= g->n_players && player >= 1)
    return (char)player;
  return '.';
}

/** @brief Daje napis opisujący stan planszy.
 * Alokuje w pamięci bufor, w którym umieszcza napis
 * zawierający tekstowy opis aktualnego stanu planszy.
 * Przykład znajduje się w pliku game_example.col. Gdy nie
 * udało się alokować pamięci, ustawia @p errno na @p
 * ENOMEM. Funkcja wywołująca musi zwolnić ten bufor.
 * @param[in] g       – wskaźnik na strukturę przechowującą
 * stan gry.
 * @return Wskaźnik na alokowany bufor zawierający napis
 * opisujący stan planszy lub NULL, jeśli nie udało się
 * alokować pamięci.
 */
char* game_board(game_t const* g) {
  if (game_exists(g)) {
    char* field =
        (char*)malloc(((index_t)g->n_col + 1) * (index_t)g->n_row + 1);
    if (field == NULL) {
      errno = ENOMEM;
      return NULL;
    }
    axis_t add = 0;
    for (axis_t row = g->n_row; row >= 1; row--) {
      for (axis_t col = 0; col < g->n_col; col++) {
        if (col == 0 && row != g->n_col) {
          field[(index_t)(g->n_row - row) * (index_t)g->n_row + add] = '\n';
          add++;
        }
        field[(index_t)(g->n_row - row) * (index_t)g->n_row + col + add] =
            ((index_t)g->board[(row - 1) * (index_t)g->n_row + col].val == 0)
                ? '.'
                : ((index_t)g->board[(row - 1) * (index_t)g->n_row + col].val +
                   '0');
      }
    }
    field[(index_t)(g->n_col + 1) * (index_t)g->n_row - 1] = '\n';
    field[((index_t)g->n_col + 1) * (index_t)g->n_row] = '\0';
    return field;
  }
  return NULL;
}
