/** @file
 * Interfejs modułu silnika gry
 *
 * @author Marcin Peczarski <marpe@mimuw.edu.pl>, Heorhii Lopatin
 * @copyright Uniwersytet Warszawski
 * @date 2023
 */

#ifndef GAME_H
#define GAME_H

#define MAX_PLAYER_COUNT 50

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t value_t;
typedef uint32_t axis_t;
typedef uint64_t index_t;
typedef uint64_t cells_t;
typedef uint32_t player_t;

typedef struct node {
  //number of a player who has put his mark on the cell, 0 otherwise
  value_t val;
  // a cell connected to this one and considered to be more recent
  index_t parent;
} node_t;

/**
 * To jest deklaracja struktury przechowującej stan gry.
 */
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

/** @brief Tworzy strukturę przechowującą stan gry.
 * Alokuje pamięć na nową strukturę przechowującą stan gry.
 * Inicjuje tę strukturę, tak aby reprezentowała początkowy stan gry.
 * Gdy nie udało się alokować pamięci, ustawia @p errno na @p ENOMEM.
 * @param[in] width   – szerokość planszy, liczba dodatnia,
 * @param[in] height  – wysokość planszy, liczba dodatnia,
 * @param[in] players – liczba graczy, liczba dodatnia,
 * @param[in] areas   – maksymalna liczba obszarów, które może zająć jeden
 *                      gracz, liczba dodatnia.
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie udało się alokować
 * pamięci lub któryś z parametrów jest niepoprawny.
 */
game_t* game_new(axis_t width, axis_t height, player_t players, uint32_t areas);

/** @brief Usuwa strukturę przechowującą stan gry.
 * Usuwa z pamięci strukturę wskazywaną przez @p g.
 * Nic nie robi, jeśli wskaźnik ten ma wartość NULL.
 * @param[in] g       – wskaźnik na usuwaną strukturę.
 */
void game_delete(game_t *g);

/** @brief Wykonuje ruch.
 * Ustawia pionek gracza @p player na polu (@p x, @p y).
 * @param[in,out] g   – wskaźnik na strukturę przechowującą stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia niewiększa od wartości
 *                      @p players z funkcji @ref game_new,
 * @param[in] x       – numer kolumny, liczba nieujemna mniejsza od wartości
 *                      @p width z funkcji @ref game_new,
 * @param[in] y       – numer wiersza, liczba nieujemna mniejsza od wartości
 *                      @p height z funkcji @ref game_new.
 * @return Wartość @p true, jeśli ruch został wykonany, a @p false,
 * gdy ruch jest nielegalny, któryś z parametrów jest niepoprawny lub
 * wskaźnik @p g ma wartość NULL.
 */
bool game_move(game_t* g, player_t player, axis_t x, axis_t y);

/** @brief Podaje liczbę pól zajętych przez gracza.
 * Podaje liczbę pól zajętych przez gracza @p player.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Liczba pól zajętych przez gracza lub zero,
 * jeśli któryś z parametrów jest niepoprawny lub wskaźnik @p g ma wartość NULL.
 */
cells_t game_busy_fields(game_t const* g, player_t player);

/** @brief Podaje liczbę pól, które jeszcze gracz może zająć.
 * Podaje liczbę wolnych pól, na których w danym stanie gry gracz @p player może
 * postawić swój pionek w następnym ruchu.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Liczba pól, jakie jeszcze może zająć gracz lub zero,
 * jeśli któryś z parametrów jest niepoprawny lub wskaźnik @p g ma wartość NULL.
 */
cells_t game_free_fields(game_t const* g, player_t player);

/** Podaje szerokość planszy.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry.
 * @return Szerokość planszy lub zero, gdy wskaźnik @p g ma wartość NULL.
 */
axis_t game_board_width(game_t const *g);

/** Podaje wysokość planszy.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry.
 * @return Wysokość planszy lub zero, gdy wskaźnik @p g ma wartość NULL.
 */
axis_t game_board_height(game_t const *g);

/** Podaje liczbę graczy.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry.
 * @return Liczba graczy lub zero, gdy wskaźnik @p g ma wartość NULL.
 */
player_t game_players(game_t const *g);

/** Daje symbole wykorzystywane w funkcji @ref game_board.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry,
 * @param[in] player  – numer gracza, liczba dodatnia niewiększa od wartości
 *                      @p players z funkcji @ref game_new.
 * @return Cyfra, litera lub inny jednoznakowy symbol gracza. Symbol oznaczający
 * puste pole, gdy numer gracza jest niepoprawny lub wskaźnik @p g ma wartość
 * NULL.
 */
char game_player(game_t const *g, player_t player);

/** @brief Daje napis opisujący stan planszy.
 * Alokuje w pamięci bufor, w którym umieszcza napis zawierający tekstowy
 * opis aktualnego stanu planszy. Przykład znajduje się w pliku game_example.c.
 * Gdy nie udało się alokować pamięci, ustawia @p errno na @p ENOMEM.
 * Funkcja wywołująca musi zwolnić ten bufor.
 * @param[in] g       – wskaźnik na strukturę przechowującą stan gry.
 * @return Wskaźnik na alokowany bufor zawierający napis opisujący stan planszy
 * lub NULL, jeśli nie udało się alokować pamięci.
 */
char* game_board(game_t const *g);

#endif /* GAME_H */