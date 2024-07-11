#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <conio.h>

struct BigObject
{
    int a, b, c, d, e, f;
    BigObject(int x) : a(x), b(x), c(x), d(x), e(x), f(x) {}
};

void f1()
{
    puts(__FUNCSIG__ " entered!");
}

int f2()
{
    puts(__FUNCSIG__ " entered!");
    return 0x123;
}

long long f3()
{
    puts(__FUNCSIG__ " entered!");
    return 0x123456789123456;
}

double f4()
{
    puts(__FUNCSIG__ " entered!");
    return 1.123;
}

BigObject f5()
{
    puts(__FUNCSIG__ " entered!");
    return BigObject(4);
}

int sum(int a, char b, int c, int d, int e, int f, int g)
{
    return a + b + c + d + e + f + g;
}

int main()
{
    f1();
    f2();
    f3();
    f4();
    f5();
    sum(1, 2, 3, 4, 5, 6, 7);
    getch();
}
