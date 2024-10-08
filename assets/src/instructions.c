/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "ru.h"

uint8_t build_CODING_BYTE(ru_operand a,
    ru_operand b, ru_operand c, ru_operand d)
{
    uint8_t cb = 0b00000000;

    cb += (a << 6);
    cb += (b << 4);
    cb += (c << 2);
    cb += d;
    return cb;
}

uint8_t *build_NOOP(void)
{
    uint8_t *noop = malloc(sizeof(uint8_t) * 2);

    assert(noop != NULL);
    memset(noop, 0x00, sizeof(uint8_t) * 2);
    return noop;
}

uint8_t *build_CREATEVAR(uint8_t vartype, uint32_t value)
{
    return NULL;
}

uint8_t *build_SETVAR(uint8_t dest_id, uint32_t value)
{
    return NULL;
}

uint8_t *build_ADD(uint32_t value1, ru_operand op_type, uint32_t value2)
{
    return NULL;
}
