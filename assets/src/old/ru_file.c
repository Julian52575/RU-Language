/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "compiler.h"
#include "ru.h"
#include <fcntl.h>

int add_to_code(compiler *comp, const uint8_t *code, uint32_t code_size)
{
    return 0;
}

int add_to_string_table(compiler *comp, const char *str, uint32_t str_size)
{
    return 0;
}

int add_to_function_table(compiler *comp, ru_function_t *fun)
{
    return 0;
}

int write_to_file(const compiler *comp)
{
    int fd = open(comp->file_name, O_RDONLY | O_CREAT, S_IRWXU);

    if (fd <= 0)
        return -1;
    write(fd, &(comp->file_content.head), sizeof(ru_header_t));
    close(fd);
    return 0;
}
