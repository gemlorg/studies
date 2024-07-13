/** @file
 * Przykładowe użycie silnika gry
 *
 * @author Marcin Peczarski <marpe@mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2023
 */

/**
 * W tym pliku nawet w wersji release chcemy korzystać z asercji.
 */
#ifdef NDEBUG
#undef NDEBUG
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"

/**
 * Tak ma wyglądać plansza po wykonaniu całego poniższego przykładu.
 */
static const char board[] =
    "1.........\n"
    "..........\n"
    "..........\n"
    "......2...\n"
    ".....1....\n"
    "..........\n"
    "..........\n"
    "1.........\n"
    "1222......\n"
    "1.........\n";

/** @brief Testuje silnik gry.
 * Przeprowadza przykładowe testy silnika gry.
 * @return Zero, gdy wszystkie testy przebiegły poprawnie,
 * a w przeciwnym przypadku kod błędu.
 */

void null_test();
void add_funcs_test();
void large_test();
void example_test();
int main() {
  null_test();
  add_funcs_test();
  large_test();
  return 0;
}

//need to watch overflows
void null_test() {
  game_t* g;
  g = game_new(100000000, 1000000, 123123, 1231231231);
  assert(g == NULL);
  g = game_new(-1, -1, -1, -1);
  assert(g == NULL);
  g = game_new(100000, 100000000, 10, 123123);
  assert(g == NULL);
  g = game_new(1, 1, 1, 0);
  assert(g == NULL);
  g = game_new(100, 100, 100, 100);
  assert(g == NULL);
  assert(!game_move(g, 1, 1, 1));
  assert(game_busy_fields(g, 1) == 0);
  assert(game_free_fields(g, 1) == 0);
  assert(game_board_height(g) == 0);
  assert(game_board_width(g) == 0);
  assert(game_players(g) == 0);
  assert(game_player(g, 1) == '.');
  assert(game_board(g) == NULL);

  g = game_new(10, 10, 10, 10);
  assert(g);
  assert(!game_move(g, 9, 9, 10));
  assert(!game_move(g, 9, 10, 9));
  assert(!game_move(g, 0, 9, 9));
  assert(!game_move(g, 11, 9, 9));
  game_delete(g);
}
void add_funcs_test() {
  //game_new();
}

void large_test() {
  game_t* g;
  g = game_new(1000, 1000, 50, 99000000);
  assert(g);
  for (int i = 999; i >= 0; i--) {
    for (int j = 999; j >= 0; j--) {
      assert(game_move(g, 1, i, j));
    }
  }
  game_delete(g);
  g = game_new(100, 100, 50, 10);
  assert(g);
  for (int i = 1; i <= 50; i++) {
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, 4 * j, i - 1));
    }
    for (int j = 0; j < 10; j++) {
      assert(!game_move(g, i, 4 * j + 2, i - 1));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, 4 * j + 1, i - 1));
    }
    for (int j = 0; j < 9; j++) {
      assert(game_move(g, i, 4 * j + 3, i - 1));
    }
    for (int j = 0; j < 9; j++) {
      assert(!game_move(g, i, 4 * j + 3, i - 1));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, 4 * j + 2, i - 1));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, 50 + j, i - 1));
    }
    for (int j = 0; j < 9; j++) {
      assert(game_move(g, i, 60 + 2 * j, i - 1));
    }
  }
  assert(!game_move(g, 1, 3, 2));
  game_delete(g);
  g = game_new(100, 100, 50, 10);
  assert(g);
  for (int i = 1; i <= 30; i++) {
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, i - 1, 4 * j));
    }
    for (int j = 0; j < 10; j++) {
      assert(!game_move(g, i, i - 1, 4 * j + 2));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, i - 1, 4 * j + 1));
    }
    for (int j = 0; j < 9; j++) {
      assert(game_move(g, i, i - 1, 4 * j + 3));
    }
    for (int j = 0; j < 9; j++) {
      assert(!game_move(g, i, i - 1, 4 * j + 3));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, i - 1, 4 * j + 2));
    }
    for (int j = 0; j < 10; j++) {
      assert(game_move(g, i, i - 1, 50 + j));
    }
    for (int j = 0; j < 9; j++) {
      assert(game_move(g, i, i - 1, 60 + 2 * j));
    }
  }
  assert(game_free_fields(g, 0) == 0);
  for (int i = 1; i <= 50; i++) {
    printf("number of free cells is curently player=%d cells free=%llu\n", i,
           game_free_fields(g, i));
  }
  char* p = game_board(g);
  assert(p);
    printf("%s\n", p);
  free(p);
  game_delete(g);
}

void example_test() {
  game_t* g;
  g = game_new(10, 10, 2, 3);
  assert(game_move(g, 1, 0, 0));
  assert(game_move(g, 1, 1, 0));
  assert(game_move(g, 1, 2, 0));
  assert(game_move(g, 1, 3, 0));
  assert(game_move(g, 1, 4, 0));
  assert(game_move(g, 1, 5, 0));
  assert(game_move(g, 1, 0, 2));
  assert(game_move(g, 1, 1, 2));
  assert(game_move(g, 1, 2, 2));
  assert(game_move(g, 1, 3, 2));
  assert(game_move(g, 1, 4, 2));
  assert(game_move(g, 1, 5, 2));
  assert(game_move(g, 1, 0, 4));
  assert(game_move(g, 1, 1, 4));
  assert(game_move(g, 1, 2, 4));
  assert(game_move(g, 1, 3, 4));
  assert(game_move(g, 1, 4, 4));
  assert(game_move(g, 1, 5, 4));
  assert(game_move(g, 1, 5, 3));
  assert(game_move(g, 1, 5, 1));
  // printf(game_board(g));
  game_delete(g);
  //    game_board(g);
  // assert(game_move(g, 1, 5, 2));
  g = game_new(0, 0, 0, 0);
  assert(g == NULL);

  g = game_new(10, 10, 2, 3);
  assert(g != NULL);

  assert(game_move(g, 1, 0, 0));
  assert(game_busy_fields(g, 1) == 1);
  assert(game_busy_fields(g, 2) == 0);
  assert(game_free_fields(g, 1) == 99);
  assert(game_free_fields(g, 2) == 99);
  assert(game_move(g, 2, 3, 1));
  assert(game_busy_fields(g, 1) == 1);
  assert(game_busy_fields(g, 2) == 1);
  assert(game_free_fields(g, 1) == 98);
  assert(game_free_fields(g, 2) == 98);
  assert(game_move(g, 1, 0, 2));
  assert(game_move(g, 1, 0, 9));
  assert(!game_move(g, 1, 5, 5));
  assert(game_free_fields(g, 1) == 6);
  assert(game_move(g, 1, 0, 1));
  assert(game_free_fields(g, 1) == 95);
  assert(game_move(g, 1, 5, 5));
  assert(!game_move(g, 1, 6, 6));
  assert(game_busy_fields(g, 1) == 5);
  assert(game_free_fields(g, 1) == 10);
  assert(game_move(g, 2, 2, 1));
  assert(game_move(g, 2, 1, 1));
  assert(game_free_fields(g, 1) == 9);
  assert(game_free_fields(g, 2) == 92);
  assert(!game_move(g, 2, 0, 1));
  assert(game_move(g, 2, 6, 6));
  assert(game_busy_fields(g, 1) == 5);
  assert(game_free_fields(g, 1) == 9);
  assert(game_busy_fields(g, 2) == 4);
  assert(game_free_fields(g, 2) == 91);
  char* p = game_board(g);
  assert(p);
  assert(strcmp(p, board) == 0);
  printf("%s\n", p);
  free(p);

  game_delete(g);
}
