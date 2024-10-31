
#ifndef INS_FACTORY_HPP
#define INS_FACTORY_HPP
#include <vector>
#include <optional>
#include <string>

#include "ru.h"

struct ruInstruction {
    public:
        uint8_t getPrefix(void) const;
        uint8_t getInfix(void) const;
        bool isCodingByteSet(void) const;
        uint8_t getCodingByte(void) const;
        uint32_t getOperandSize(void) const;
        uint32_t getTotalSize(void) const;
        bool isFunctionStart(void) const;
        std::string getFunctionName(void) const;

        void setCodingByte(uint8_t codingByte);
        void setOpcode(uint16_t opcode);
        void setFunctionStart(std::string functionName);

        void addOperandByte(uint8_t value);
        void addOperandByte(uint32_t value);
        uint8_t operator[](std::size_t index);
    public:
        ruInstruction copy(void) const;
    private:
        uint8_t _opcode[RU_SIZE_OPCODE];
        std::optional<uint8_t> _codingByte;
        std::vector<uint8_t> _bytes;
        std::optional<std::string> _functionStartName;
};


class instructionFactory {
    public:
        instructionFactory() = default;
        ~instructionFactory() = default;

    public:
        ruInstruction createVar(uint32_t varType, uint32_t operand2);
        ruInstruction add(RU_OPERAND operand1Type, uint32_t operand1,
            RU_OPERAND operator2Type, uint32_t operand2);
    protected:

    private:
        uint8_t getCodingByte(RU_OPERAND a,
            RU_OPERAND b, RU_OPERAND c, RU_OPERAND d);
};


#endif
