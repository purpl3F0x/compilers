/**
 * @file io.c
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */

#include "io.h"

#include <limits.h>
#include <stdio.h>

void writeInteger(int n)
{
    char buffer[16];
    int  i = 0;

    if (n == INT_MIN) {
        fputs("-2147483648", stdout);
        return;
    }
    if (n == 0) {
        putchar('0');
        return;
    }
    if (n < 0) {
        putchar('-');
        n = -n;
    }

    while (n > 0) {
        buffer[i++] = n % 10 + '0';
        n /= 10;
    }

    while (i > 0) {
        putchar(buffer[--i]);
    }
}

void writeByte(const uint8_t b)
{
    const char hex[16] = "0123456789ABCDEF";

    putchar('0');
    putchar('x');
    putchar(hex[b >> 4u]);
    putchar(hex[b & 0x0Fu]);
}

void writeChar(const uint8_t c)
{
    putchar(c);
}

void writeString(const char c[])
{
    fputs(c, stdout);
}

int readInteger()
{
    char c, sign = 1;
    int  res = 0;

    if ((c = getchar()) == '-') {
        sign = -1;
        c = getchar();
    } else if (c == '+') {
        c = getchar();
    } else if (c < '0' || c > '9') {
        return 0;
    }

    do {
        res = res * 10 + (c - '0');
        // check for overflow
        if (res < 0) {
            if (res == INT_MIN) { // handle edge case of INT_MIN
                if ((c = getchar()) < '0' || c > '9')
                    return res;
            }
            return 0;
        }

    } while ((c = getchar()) >= '0' && c <= '9');

    return sign * res;
}

char readByte()
{
    return getchar();
}

char readChar()
{
    return getchar();
}

void readString(const int n, char s[])
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"

    (void)fgets(s, n, stdin);

#pragma GCC diagnostic pop
}