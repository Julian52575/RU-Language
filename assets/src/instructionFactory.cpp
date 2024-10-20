#include "instructionFactory.hpp"
#include "ru.h"
#include <cstdint>
#include <cstdlib>
#include <exception>

void ruInstruction::setOpcode(uint16_t opcode)
{
    uint8_t *cast = (uint8_t *) &opcode;

    this->_opcode[0] = cast[0];
    this->_opcode[1] = cast[1];
}
uint8_t ruInstruction::getPrefix(void) const
{
    return this->_opcode[0];
}
uint8_t ruInstruction::getInfix(void) const
{
    return this->_opcode[1];
}

bool ruInstruction::isCodingByteSet(void) const
{
    return this->_codingByte.value_or(false);
}
void ruInstruction::setCodingByte(uint8_t codingByte)
{
    this->_codingByte = codingByte;
}
void ruInstruction::setFunctionStart(std::string functionName)
{
    this->_functionStartName = functionName;
}

uint8_t ruInstruction::getCodingByte(void) const
{
    return this->_codingByte.value_or(0xff);
}

void ruInstruction::addOperandByte(uint8_t value)
{
    this->_bytes.push_back(value);
}
void ruInstruction::addOperandByte(uint32_t value)
{
    for (int i = 0; i < 4; i++) {
        this->_bytes.push_back(((uint8_t *) (&value))[i]);
    }
}
uint32_t ruInstruction::getOperandSize(void) const
{
    if (this->_bytes.size() > (uint32_t) -1) {
        throw std::exception();
    }
    return this->_bytes.size();
}
uint32_t ruInstruction::getTotalSize(void) const
{
    uint32_t total = 0;

    total += RU_SIZE_OPCODE;
    if (this->_codingByte.has_value()) {
        total += RU_SIZE_CODING_BYTE;
    }
    total += this->getOperandSize();
    return total;
}

uint8_t ruInstruction::operator[](std::size_t index)
{
    if (index > this->_bytes.size()) {
        throw std::exception();
    }
    return this->_bytes[index];
}

ruInstruction ruInstruction::copy(void) const
{
    ruInstruction result;

    result.setOpcode((this->_opcode[0] << 8) + this->_opcode[1]);
    if (this->isCodingByteSet() == true) {
        result.setCodingByte(this->_codingByte.value());
    }
    for (uint8_t i : this->_bytes) {
        result.addOperandByte(i);
    }
    return result;
}

bool ruInstruction::isFunctionStart(void) const
{
    if (this->_functionStartName.has_value() == true) {
        return true;
    }
    return false;
}
std::string ruInstruction::getFunctionName(void) const
{
    if (this->isFunctionStart() == true) {
        return this->_functionStartName.value();
    }
    uint64_t ptr = (uint64_t) (&this->_bytes);
    std::string name = "Undefined-";

    name += ptr;
    return this->_functionStartName.value_or(name);
}

////////////////////////////////////////////////////////////////////////////////
ruInstruction instructionFactory::createVar(uint8_t varType, uint32_t operand2)
{
    ruInstruction result;

    result.setOpcode(0x0100);
    result.addOperandByte(varType);
    result.addOperandByte(operand2);
    return result;
}

ruInstruction instructionFactory::add(RU_OPERAND operand1Type, uint32_t operand1,
            RU_OPERAND operator2Type, uint32_t operand2)
{
#warning Code add instruction
    ruInstruction result;

    return result;
}

uint8_t
instructionFactory::getCodingByte(RU_OPERAND a,
    RU_OPERAND b, RU_OPERAND c, RU_OPERAND d)
{
    uint8_t cb = 0b00000000;

    cb += (a << 6);
    cb += (b << 4);
    cb += (c << 2);
    cb += d;
    return cb;
}
