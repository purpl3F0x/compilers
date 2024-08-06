/**
 * @file type_conversions.c
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief  Type conversion functions
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */

#include "type_conversions.h"

int extend(const uint8_t b)
{
    return (int)b;
}

char shrink(const int i)
{
    return (char)i;
}