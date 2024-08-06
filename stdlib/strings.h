/**
 * @file strings.h
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief  String manipulation functions
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */

#pragma once

/**
 * @brief Calculates the length of a string
 *
 * @param s
 * @return int
 */
int strlen(const char *s);

/**
 * @brief Compares two strings
 *
 * @param s1
 * @param s2
 * @return int  0 if the strings are equal, a negative number if s1 < s2, a positive number if s1 > s2
 */
int strcmp(const char *s1, const char *s2);

/**
 * @brief Copies a string
 *
 * @param trg
 * @param src
 */
void strcpy(char *trg, const char *src);

/**
 * @brief Concatenates two strings
 *
 * @param trg
 * @param src
 */
void strcat(char *trg, const char *src);