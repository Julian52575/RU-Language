/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <string.h>

#define RU_HEADER_SIZE 64
#define RU_HEADER_PADDING_SIZE 32
typedef struct ru_header {
    uint8_t magic[5];
    uint16_t checksum;
    uint8_t file_version;
    uint32_t function_number;
    uint32_t string_table_offset;
    uint32_t string_number;
    uint32_t code_offset;
    uint8_t padding[32];
} ru_header;

typedef struct ru_function {
    uint32_t name_index;
    uint32_t code_offset;
    uint32_t instruction_count;
    uint32_t unused;
} ru_function;

typedef enum ru_operand {
    NA = 0b00,
    UNUSED = 0b01,
    CONSTANT_TYPE = 0b10,
    VAR_REFERENCE = 0b11
} ru_operand;


