#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef WIERSZE
#define WIERSZE 22
#endif

#ifndef KOLUMNY
#define KOLUMNY 80
#endif

static int move_x = 1;
static int move_y = 1;

struct point {
    int x;
    int y;
};

int extend(int size) {
    if(size >= INT8_MAX / 2 - 2)
        return INT8_MAX;
    return size * 2 + 2; }

void init_field(struct point **a, int *n) {
    struct point *result = NULL;
    int size = 0;
    int i = 0;
    char c;
    int w;
    int l;
    while ((char)getchar() == '/' && (c = (char)getchar()) != '\n') {
        ungetc(c, stdin);
        scanf("%d", &w);
        while ((c = (char)getchar()) != '\n') {
            if (c != ' ') {
                ungetc(c, stdin);
                scanf("%d", &l);
                if (i == size) {
                    size = extend(size);
                    result = realloc(result, (size_t)size * sizeof *result);
                }
                struct point p;
                p.y = w;
                p.x = l;
                result[i] = p;
                i++;
            }
        }
    }
    result = realloc(result, (size_t)i * sizeof *result);
    *a = result;
    *n = i;
}

void print_field(struct point *a, int len) {
    int i = 0;
    for (int y = move_y; y < WIERSZE + move_y; y++) {
        for (int x = move_x; x < KOLUMNY + move_x; x++) {
            while (i < len && (a[i].y < y || (a[i].y == y && a[i].x < x))) {
                i++;
            }
            if (i < len && a[i].x == x && a[i].y == y)
                printf("0");
            else
                printf(".");
        }
        printf("\n");
    }
    for (int b = 0; b < KOLUMNY; b++) {
        printf("=");
    }
    printf("\n");
}

int comp(const void *elem1, const void *elem2) {
    struct point *f = ((struct point *)elem1);
    struct point *s = ((struct point *)elem2);
    if (f->y > s->y || (f->y == s->y && f->x > s->x))
        return 1;
    if (f->y < s->y || (f->y == s->y && f->x < s->x))
        return -1;
    return 0;
}

int count_neighbours(int x, int y, struct point *arr, int len) {
    int count = 0;
    for (int j = 0; j < len; j++) {
        if (abs(x - arr[j].x) <= 1 && abs(y - arr[j].y) <= 1 &&
            !(x == arr[j].x && y == arr[j].y)) {
            count++;
        }
    }
    return count;
}

bool contains(struct point *result, int i, int x, int y) {
    if (result == NULL) {
        return false;
    }
    for (int j = 0; j < i; j++) {
        if (result[j].x == x && result[j].y == y) {
            return true;
        }
    }
    return false;
}
bool should_be_alive(int x, int y, struct point *arr, int a_l,
                     struct point *result, int r_l) {
    if (contains(result, r_l, x, y)) {
        return false;
    }
    int n = count_neighbours(x, y, arr, a_l);
    if (n == 3)
        return true;
    if (n == 2 && contains(arr, a_l, x, y))
        return true;
    return false;
}

void move(struct point **arr, int *len, int count) {

    for (int a = 0; a < count; a++) {
        struct point *result = NULL;
        int size = 0;
        int i = 0;
        for (int j = 0; j < *len; j++) {
            for (int x = (*arr)[j].x - 1; x <= (*arr)[j].x + 1; x++) {
                for (int y = (*arr)[j].y - 1; y <= (*arr)[j].y + 1; y++) {
                    if (should_be_alive(x, y, *arr, *len, result, i)) {
                        if (i == size) {
                            size = extend(size);
                            result = realloc(result, (size_t)size * sizeof(struct point));
                        }
                        struct point p;
                        p.x = x;
                        p.y = y;
                        result[i] = p;
                        i++;
                    }
                }
            }
        }
        free(*arr);
        result = realloc(result, (size_t)i * sizeof(struct point));
        *arr = result;
        *len = i;
        qsort(*arr, (size_t)*len, sizeof(struct point), comp);
    }
    print_field(*arr, *len);
}

void change_screen(int w, int k) {
    move_x = k;
    move_y = w;
}

void get_info(struct point *arr, int len) {
    int prev_y = arr[0].y;
    printf("/%d", arr[0].y);
    for (int i = 0; i < len; i++) {
        if (arr[i].y != prev_y) {
            printf("\n/%d", arr[i].y);
        }
        printf(" %d", arr[i].x);
        prev_y = arr[i].y;
    }
    printf("\n/\n");
    print_field(arr, len);
}

void read_commands(struct point *arr, int len) {
    char z;
    int a;
    int b;
    while ((z = (char)getchar()) != '.') {
        if (z == '\n') {
            move(&arr, &len, 1);
        } else {
            ungetc(z, stdin);
            scanf("%d", &a);
            if ((z = (char)getchar()) == '\n') {
                if (a == 0) {
                    get_info(arr, len);
                } else {
                    move(&arr, &len, a);
                }
            } else {
                ungetc(z, stdin);
                scanf("%d", &b);
                change_screen(a, b);
                move(&arr, &len, 0);
                getchar();
            }
        }
    }
    free(arr);
}

int main(void) {
    struct point *arr;
    int len;
    init_field(&arr, &len);
    move(&arr, &len, 0);
    read_commands(arr, len);
}
