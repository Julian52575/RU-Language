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
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define RU_SIZE_OPCODE  2
#define RU_SIZE_CODING_BYTE 1

#define RU_SIZE_OPRAND_CONSTANT 4
#define RU_SIZE_OPRAND_VAR_ID    4
#define RU_SIZE_OPRAND_VAR_TYPE 1

#define RU_HEADER_SIZE 64
#define RU_HEADER_PADDING_SIZE 32

typedef struct ru_header_s {
    uint8_t magic[5];
    uint8_t checksum[2];
    uint8_t file_version;
    uint32_t function_number;
    uint32_t string_table_offset;
    uint32_t string_number;
    uint32_t code_offset;
    uint32_t entrypointFunctionIndex;
    uint8_t padding[36];
} ru_header_t;

typedef struct ru_function_s {
    uint32_t name_index;
    uint32_t code_offset;
    uint32_t size;
} ru_function_t;

typedef enum RU_OPERAND {
    RU_OPERAND_NA = 0b00,
    RU_OPERAND_UNUSED = 0b01,
    RU_OPERAND_CONSTANT_TYPE = 0b10,
    RU_OPERAND_VAR_ID = 0b11
} RU_OPERAND;

#define RU_VARTYPE_INT      (uint8_t) 0X01
#define RU_VARTYPE_STR      (uint8_t) 0x02
#define RU_VARTYPE_FUN_PTR  (uint8_t) 0x03
