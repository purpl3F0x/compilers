/**
 * @file io.h
 * @author Stavros Avramidis (stavros9899@gmail.com)
 * @brief
 * @version 0.1
 * @date 2024-07-26
 *
 * @copyright Copyright (c) 2024
 *
 */

#pragma once

#include <inttypes.h>

/******************************************
 *           *Output functions            *
 ******************************************/

/**
 * @brief  Writes an integer to the standard output
 *
 * @param n
 */
void writeInteger(int n);

/**
 * @brief  Writes a byte to the standard output as 0xXX
 *
 * @param b
 */
void writeByte(const uint8_t b);

/**
 * @brief  Writes a character to the standard output
 *
 * @param b
 */
void writeChar(const uint8_t b);

/**
 * @brief  Writes a string to the standard output
 *
 * @param s
 */
void writeString(const char s[]);

/******************************************
 *            *Input functions            *
 ******************************************/

/**
 * @brief  Reads an integer from the standard input, returns 0 if no integer is found
 *
 * @return int
 */
int readInteger();

/**
 * @brief  Reads a byte from the standard input, returns 0 if no byte is found
 *
 * @return uint8_t
 */
char readByte();

/**
 * @brief  Reads a character from the standard input, returns 0 if no character is found
 *
 * @return uint8_t
 */
char readChar();

/**
 * @brief  Reads a string from the standard input
 *
 * @param n read at most n characters
 * @param s destination buffer
 */
void readString(const int n, char s[]);
