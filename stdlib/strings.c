/**
 * @file strings.c
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief  String manipulation functions
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */
#include "strings.h"

#include <inttypes.h>

static void memcpy(char dst[], const char src[], int n)
{
    char       *d = dst;
    const char *s = src;

    // a simple optimization never hurt anyone
#ifdef __SIZEOF_INT128__
    while (n >= 16) {
        *(unsigned __int128 *)d = *(unsigned __int128 *)s;
        d += 16;
        s += 16;
        n -= 16;
    }
#else
    while (n >= 8) {
        *(uint64_t *)d = *(uint64_t *)s;
        d += 8;
        s += 8;
        n -= 8;
    }
#endif

    while (n--) {
        *d++ = *s++;
    }
}

int strlen(const char *s)
{
    const char *s_ptr = s;
    for (; *s_ptr; ++s_ptr) {
        ;
    }

    return (int)(s_ptr - s);
}

int strcmp(const char *s1, const char *s2)
{
    while (*s1 == *s2++)
        if (*s1++ == '\0')
            return (0);
    return (*(const unsigned char *)s1 - *(const unsigned char *)(s2 - 1));
}

void strcpy(char *trg, const char *src)
{
    const int len = strlen(src);
    memcpy(trg, src, len + 1);
}

void strcat(char *trg, const char *src)
{
    const int trg_len = strlen(trg);
    const int src_len = strlen(src);

    memcpy(trg + trg_len, src, src_len + 1);
}