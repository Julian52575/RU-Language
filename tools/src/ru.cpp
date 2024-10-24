#include "ru.hpp"
#include "instructionFactory.hpp"
#include "ru.h"
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>


int writeBigEndian(int fd, uint16_t i)
{
    uint8_t *cast = ((uint8_t *) &i);

    write(fd, &cast[1], 1);
    write(fd, &cast[0], 1);
    return 0;
}

int writeBigEndian(int fd, uint32_t i)
{
    uint8_t *cast = ((uint8_t *) &i);

    write(fd, &cast[3], 1);
    write(fd, &cast[2], 1);
    write(fd, &cast[1], 1);
    write(fd, &cast[0], 1);
    return 0;
}

int writeBigEndian(int fd, ru_header_t header)
{
    write(fd, "CROUS", 5);
    write(fd, &header.checksum, 2);
    write(fd, &header.file_version, 1);
    writeBigEndian(fd, header.function_number);
    writeBigEndian(fd, header.string_table_offset);
    writeBigEndian(fd, header.string_number);
    writeBigEndian(fd, header.code_offset);
    writeBigEndian(fd, header.entrypointFunctionIndex);
    uint8_t padding = 0x00;
    for (int i = 0; i < 36; i++) {
        write(fd, &padding, 1);
    }
    return 0;
}

int writeBigEndian(int fd, ru_function_t fun)
{
/*    uint32_t name_index;
    uint32_t code_offset;
    uint32_t size;*/
    uint8_t *cast = ((uint8_t *) &(fun.name_index));

    write(fd, &cast[3], 1);
    write(fd, &cast[2], 1);
    write(fd, &cast[1], 1);
    write(fd, &cast[0], 1);

    cast = ((uint8_t *) &(fun.code_offset));
    write(fd, &cast[3], 1);
    write(fd, &cast[2], 1);
    write(fd, &cast[1], 1);
    write(fd, &cast[0], 1);

    cast = ((uint8_t *) &(fun.size));
    write(fd, &cast[3], 1);
    write(fd, &cast[2], 1);
    write(fd, &cast[1], 1);
    write(fd, &cast[0], 1);
    return 0;
}

//   code section
void ruSectionCode::addInstruction(const ruInstruction& instruction)
{
    ruInstruction tmp = instruction.copy();

    this->_instructionVector.push_back(tmp);
    this->_totalSize += instruction.getTotalSize();
}

int ruSectionCode::writeToFile(int fd) const
{
    uint8_t tmp = 0x00;

    for (ruInstruction current : this->_instructionVector) {
        tmp = current.getPrefix();
        write(fd, &tmp, 1);
        tmp = current.getInfix();
        write(fd, &tmp, 1);
        if (current.isCodingByteSet()) {
            tmp = current.getCodingByte();
            write(fd, &tmp, 1);
        }
        for (uint32_t i = 0; i < current.getOperandSize(); i++) {
            tmp = current[i];
            write(fd, &tmp, 1);
        }
    }
    return 0;
}
uint32_t ruSectionCode::getNumberOfElement(void) const
{
    return (uint32_t) this->_instructionVector.size();
}

//  strTab
void ruSectionString::addString(const char *str)
{
    if (this->_stringVector.size() == 0) {
        this->_stringVector.push_back("\0");
        this->_totalSize += 1;
    }
    this->_stringVector.push_back(str);
    this->_totalSize += strlen(str) + 1;
}
int ruSectionString::writeToFile(int fd) const
{
    uint8_t sep = 0x00;

    for (std::string current : this->_stringVector) {
        write(fd, current.c_str(), (size_t) current.size());
        write(fd, &sep, 1);
    }
    return 0;
}
uint32_t ruSectionString::getNumberOfElement(void) const
{
    return this->_stringVector.size();
}
const std::string& ruSectionString::getStringFromIndex(uint32_t index) const
{
    return this->_stringVector[index];
}
//   function section
void ruSectionFunction::addFunction(const ru_function_t& fun)
{
    ru_function_t tmp;

    tmp.code_offset = fun.code_offset;
    tmp.size = fun.size;
    tmp.name_index = fun.name_index;
    this->_functionVector.push_back(tmp);
    this->_totalSize += sizeof(ru_function_t);
}
int ruSectionFunction::writeToFile(int fd) const
{
    size_t structSize = sizeof(uint32_t) * 3;

    for (ru_function_t current : this->_functionVector) {
        writeBigEndian(fd, current);
    }
    return 0;
}
uint32_t ruSectionFunction::getNumberOfElement(void) const
{
    return this->_functionVector.size();
}
void ruSectionFunction::updateInstructionCount(const ruSectionCode& codeSection,
    const ruSectionString& strTab)
{
#warning update function size
    return;
}

////////////////////////////////////////////////////////////////////////////////

ruFile::ruFile()
{
    memset(&this->_header, 0x00, sizeof(ru_header_t));
    this->_header.magic[0] = 'C';
    this->_header.magic[1] = 'R';
    this->_header.magic[2] = 'O';
    this->_header.magic[3] = 'U';
    this->_header.magic[4] = 'S';
    this->_header.file_version = 0x01;
    this->_header.checksum[0] = ((int64_t) &this->_header) % 42;
    this->_header.checksum[1] = ((int64_t) &this->_header) % 13;
    this->_header.entrypointFunctionIndex = 0;
}

void ruFile::setFileName(const char *fileName)
{
    this->_fileName = fileName;
}

int ruFile::writeToFile(void) const
{
    int fd = open(this->_fileName, O_CREAT | O_WRONLY, S_IRWXG | S_IRWXU);

    if (fd == -1) {
        perror("open");
        return -1;
    }
    writeBigEndian(fd, this->_header);
    this->_functionTable.writeToFile(fd);
    this->_stringTable.writeToFile(fd);
    this->_codeSection.writeToFile(fd);
    close(fd);
    return 0;
}

void ruFile::addInstruction(const ruInstruction& instruction)
{
    if (instruction.isFunctionStart() == false) {
        goto append;
    }
    ru_function_t fun;

    fun.code_offset = this->_codeSection.getTotalSize();
    fun.name_index = this->_stringTable.getNumberOfElement();
    if (fun.name_index == 0) {
        fun.name_index = 1;
    }
    this->addString(instruction.getFunctionName().c_str());
    fun.size = 0xFF;
    this->addFunction(fun);
append:
    this->_codeSection.addInstruction(instruction);
}

void ruFile::addFunction(const ru_function_t& newFunction)
{
    this->_header.function_number += 1;
    if (this->_stringTable.getStringFromIndex(newFunction.name_index) == "main") {
        this->_header.entrypointFunctionIndex = this->_functionTable.getNumberOfElement();
    }
    this->_functionTable.addFunction(newFunction);
    this->headerUpdateOffsets();
}

void ruFile::addString(const char *str)
{
    this->_header.string_number += 1;
    this->_stringTable.addString(str);
    this->headerUpdateOffsets();
}

void ruFile::headerUpdateOffsets(void)
{
    this->_header.string_table_offset = sizeof(ru_header_t) + this->_functionTable.getTotalSize();
    this->_header.code_offset = this->_header.string_table_offset + this->_stringTable.getTotalSize();
}
