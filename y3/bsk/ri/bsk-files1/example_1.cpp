#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// #include <conio.h>

// Any bugs here?

int main()
{
    char s[16];
    printf("Password: ");
    scanf("%16s", s);
    std::reverse(s, s+strlen(s));
    if (!strcmp(s, "qwerty"))
        puts("Correct!");
    else
        puts("Wrong!");
    getchar();
}
