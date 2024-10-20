/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "ru.h"
#include <assert.h>

uint8_t build_CODING_BYTE(RU_OPERAND a,
    RU_OPERAND b, RU_OPERAND c, RU_OPERAND d)
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
    uint8_t *ins = malloc(sizeof(uint8_t) * (2 + 1 + 1 + 4));

    assert(ins != NULL);
    ins[0] = 0x01;
    ins[1] = 0x00;
    ins[2] = build_CODING_BYTE(CONSTANT_TYPE, NA, NA, NA);
    ins[3] = vartype;
    for (int i = 0; i < 3; i++) {
        ins[7 - i] = ((&value)[i]);
    }
    return NULL;
}

uint8_t *build_SETVAR(uint8_t dest_id, uint32_t value)
{
    return NULL;
}

uint8_t *build_ADD(uint32_t value1, RU_OPERAND op_type, uint32_t value2)
{
    return NULL;
}
