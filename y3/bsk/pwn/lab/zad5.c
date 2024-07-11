// gcc -o zad5 zad5.c

#include <stdio.h>
#include <string.h>
#include <unistd.h>

void echo(void) {
    while (1) {
        char buf[0x20] = { 0 };
        read(0, buf, 0x200);
        if (!strncmp(buf, "exit", 4)) {
            return;
        }
        printf("%s", buf);
    }
}

int main(void) {
    setbuf(stdout, NULL);
    echo();
    return 0;
}
