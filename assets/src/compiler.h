/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "ru.h"

typedef struct ru_file {
    ru_header *head;
    ru_function *fun_table;
    char *string_table;
    uint8_t *code;
} ru_file;

typedef struct compiler {
    const char *file_name;
    ru_file file_content;
    uint32_t fun_table_size;
    uint32_t string_table_size;
} compiler;


int add_to_code(compiler *comp, const uint8_t *code, uint32_t code_size);
int add_to_string_table(compiler *comp, const char *str, uint32_t str_size);
int add_to_function_table(compiler *comp, ru_function *fun);
int write_to_file(const compiler *comp);
