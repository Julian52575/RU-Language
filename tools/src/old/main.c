/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "ru.h"
#include "instructions.h"
#include "compiler.h"

static void
worker(compiler_t *comp)
{
    uint8_t *code = NULL;

    code = build_CREATEVAR(RU_VARTYPE_INT, 15);
    comp_add_to_code(comp, code, uint32_t code_size);
}

int main(int ac, char * const *av)
{
    compiler_t *comp = NULL;
    char *file_name = "a.ru";

    if (ac > 1) {
        file_name = av[1];
    }
    comp = init_compiler(av[1]);
    worker(comp);
    free_compiler(comp);
    return 0;
}
