/**
 * @file type_conversions.h
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief  Type conversion functions
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */

#pragma once

#include <inttypes.h>

/**
 * @brief Extends a byte to an integer
 *
 * @param b
 * @return int
 */
int extend(const uint8_t b);

/**
 * @brief Shrinks an integer to a byte
 *
 * @param i
 * @return char
 */
char shrink(const int i);
