#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#ifndef WIERSZE
#define WIERSZE 10
#endif

#ifndef KOLUMNY
#define KOLUMNY 15
#endif

#ifndef RODZAJE
#define RODZAJE 4
#endif

/// check if the following block has neighbours with the same value
bool has_group(int x, int y, char *field[KOLUMNY]) {
    return field[x][y] != '.' &&
           ((x > 0 && field[x - 1][y] == field[x][y]) ||
            (x < KOLUMNY - 1 && field[x + 1][y] == field[x][y]) ||
            (y > 0 && field[x][y - 1] == field[x][y]) ||
            (y < KOLUMNY - 1 && field[x][y + 1] == field[x][y]));
}

// switch elements of a column i with an empty column
void switch_columns(int i, int temp_i, char *field[KOLUMNY]) {
    for (int j = 0; j < WIERSZE; j++) {
        field[temp_i][j] = field[i][j];
        field[i][j] = '.';
    }
}

// print the field
void print(char *field[KOLUMNY]) {
    for (int r = 0; r < WIERSZE; r++) {
        for (int k = 0; k < KOLUMNY; k++) {
            printf("%c", field[k][r]);
        }
        printf("\n");
    }
}

// read field from input
void read(char *field[KOLUMNY]) {
    for (int r = 0; r < WIERSZE; r++) {
        for (int k = 0; k < KOLUMNY; k++) {
            field[k][r] = (char)getchar();
        }
        getchar();
    }
}

// recursive function to remove a group of blocks
void remove_pos(int x, int y, char given, char *field[KOLUMNY]) {
    if (x >= 0 && y >= 0 && x < KOLUMNY && y < WIERSZE && field[x][y] != '.' &&
        field[x][y] == given) {
        field[x][y] = '.';
        remove_pos(x + 1, y, given, field);
        remove_pos(x - 1, y, given, field);
        remove_pos(x, y + 1, given, field);
        remove_pos(x, y - 1, given, field);
    }
}

void order_field(char *field[KOLUMNY]) {
    int temp_j;
    int temp_i;
    // drop top blocks
    for (int i = 0; i < KOLUMNY; i++) {
        for (int j = WIERSZE - 1; j >= 0; j--) {
            temp_j = j;
            while (temp_j < WIERSZE - 1 && field[i][temp_j] != '.' &&
                   field[i][temp_j + 1] == '.') {
                field[i][temp_j + 1] = field[i][temp_j];
                field[i][temp_j] = '.';
                temp_j++;
            }
        }
        // drop columns
        if (i > 0 && field[i][WIERSZE - 1] != '.') {
            temp_i = i;
            while (temp_i > 0 && field[temp_i - 1][WIERSZE - 1] == '.') {
                temp_i--;
            }
            if (temp_i < i) {
                switch_columns(i, temp_i, field);
            }
        }
    }
}

int main(int argc, char *argv[]) {
    // check input arguments
    if (argc != 3)
        return 1;

    // initialize the field
    char *field[KOLUMNY];
    for (int i = 0; i < KOLUMNY; i++) {
        field[i] = (char *)malloc(WIERSZE * sizeof(char));
        memset(field[i], '.', WIERSZE);
    }
    // read the input
    read(field);

    // find + delete group
    if (has_group(atoi(argv[2]), atoi(argv[1]), field))
        remove_pos(atoi(argv[2]), atoi(argv[1]),
                   field[atoi(argv[2])][atoi(argv[1])], field);

    // sort  the field
    order_field(field);

    // print the field
    print(field);

    //free allocated memory
    for (int i = 0; i < KOLUMNY; i++)
        free(field[i]);
}