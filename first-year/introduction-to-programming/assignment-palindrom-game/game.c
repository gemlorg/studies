#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#ifndef WIERSZE
#define WIERSZE 8
#endif

#ifndef KOLUMNY
#define KOLUMNY 8
#endif

#ifndef DLUGOSC
#define DLUGOSC 5
#endif

int  max(int a, int b) {
    return a > b ? a : b;
}

int min(int a , int b) {
    return a < b ? a : b;
}
void print_field( char* field[KOLUMNY]){
   for(int i = WIERSZE - 1; i >= 0; i--){
        for(int j = 0; j < KOLUMNY; j++){
            printf(" %c", field[j][i]);
        }
        printf("\n");
   }
   for(int i = 0; i < KOLUMNY; i++){
        printf(" %c", (char) i + 97);
   }
   printf("\n");
}

//checks if a given sequence is a palindrome
bool is_palindrom(int col, int row, int x, int y, char* field[KOLUMNY]){
    for(int i = 0; i < DLUGOSC / 2; i++){
        if(field[col + x * i ][row + y * i]\
                != field[col + (DLUGOSC - 1 - i) * x]\
                [row + (DLUGOSC - 1 - i ) * y]\
                || field[col + x * i][row + y * i] == '-'){
            return false;
        }
    }
    if(DLUGOSC % 2 == 1\
            && field[col + x * (DLUGOSC / 2)]\
            [row + y * (DLUGOSC / 2 )] == '-'){
        return false;
    }
    return true;
}

//checks if after the last input someone has won
bool win(char input, char* field[KOLUMNY], int* filled){
    int col;
    col = (int) (input -  'a');
    int row;
    row = max(filled[col] - 1, 0);
    //check row + diag
    for(int i = max(col - DLUGOSC, 0); i <= min(col, KOLUMNY - DLUGOSC);
i++){
        if(is_palindrom(i, row, 1, 0, field)){
            return true;
        }
        if(row - (col - i) >= 0\
                && row - col + i + DLUGOSC - 1 < WIERSZE\
                 && is_palindrom(i, row - col + i, 1, 1, field )){
            return true;
        }
        if(row + col - i < WIERSZE\
                && row + col - i - DLUGOSC + 1 >= 0\
                &&is_palindrom(i, row + col - i, 1, -1, field)){
            return true;
            }
    }
    //check collumn
    for(int j = max(row - DLUGOSC, 0);\
            j <= min(row, WIERSZE - DLUGOSC); j++){
        if(is_palindrom(col, j, 0, 1, field)){
            return true;
        }
     }
    return false;
}

//add a mark on a field
void move(char input, char* field[KOLUMNY], int* filled, int\
                current_player){
        int collumn;
        collumn  =  (int) input - (int) 'a';
        field[collumn][filled[collumn]] =  \
        (current_player == 1 ? '1' : '2');
        filled[collumn]++;
}

int main(void){
    //initialize the field
    char* field[KOLUMNY];
    for(int i = 0; i < KOLUMNY; i++){
        field[i] = (char*) calloc(WIERSZE , sizeof(char));
        memset(field[i], '-', WIERSZE);
    }
    //initialize arr with info about how filled the rows are
    int* filled;
    filled = (int*) calloc(KOLUMNY , sizeof(int));
    memset(filled, 0, KOLUMNY);
    //first output
    int current_player = 1;
    char input = ' ';
    bool w = false;
    print_field(field);
    printf("1:\n");
    //start the game
    while(input != '.' && !w){
        //get next char
        input = (char) getchar();
         while(input == '\n'){
             input = (char) getchar();
         }
        if(input != '.'){
            //make move
            move(input, field, filled, current_player);
            //print field
            print_field(field);
            //print next player.
            w = win(input, field, filled);
            if(w){
                printf("%d!\n", current_player);
            }else{
                current_player = 3 - current_player;
                printf("%d:\n", current_player);
            }
        }
    }
    free(filled);
    for(int i = 0; i < KOLUMNY; i++){
        free(field[i]);
    }
    return 0;
}
