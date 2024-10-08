/*
** EPITECH PROJECT, 2024
** glados
** File description:
** ru
*/
#include "ru.h"

uint8_t build_CODING_BYTE(ru_operand a,
    ru_operand b, ru_operand c, ru_operand d);

uint8_t *build_NOOP(void);

uint8_t *build_CREATEVAR(uint8_t vartype, uint32_t value);

uint8_t *build_SETVAR(uint8_t dest_id, uint32_t value);

uint8_t *build_ADD(uint32_t value1, ru_operand op_type, uint32_t value2);

