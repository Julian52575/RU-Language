
#ifndef RU_HPP
#define RU_HPP
#include <string>
#include <vector>
#include <utility>

#include "ru.h"
#include "instructionFactory.hpp"


struct ruSection {
    public:
        uint32_t getTotalSize(void) const {
            return this->_totalSize;
        }
        virtual uint32_t getNumberOfElement(void) const = 0;

    protected:
        uint32_t _totalSize = 0x00;
};

struct ruSectionCode : public ruSection {
    public:
        void addInstruction(const ruInstruction& instruction);
        int writeToFile(int fd) const;
        uint32_t getNumberOfElement(void) const override;

    private:
        std::vector<ruInstruction> _instructionVector;
};

struct ruSectionString : public ruSection {
    public:
        void addString(const char *str);
        int writeToFile(int fd) const;
        uint32_t getNumberOfElement(void) const override;
        const std::string& getStringFromIndex(uint32_t index) const;

    private:
        std::vector<std::string> _stringVector;
        std::string _empty = "\0";
};

struct ruSectionFunction : public ruSection {
    public:
        void addFunction(const ru_function_t& fun);
        int writeToFile(int fd) const;
        uint32_t getNumberOfElement(void) const override;
        void updateInstructionCount(const ruSectionCode& codeSection,
            const ruSectionString& strTab);

    private:
        std::vector<ru_function_t> _functionVector;
};

class ruFile {
    public:
        ruFile();
        ~ruFile() = default;
        void setFileName(const char *fileName);
        int writeToFile(void) const;

        void addInstruction(const ruInstruction& ins);
        void addString(const char *str);
    private:
        const char *_fileName = "a.ru";
        ru_header_t _header;
        ruSectionFunction _functionTable;
        ruSectionString _stringTable;
        ruSectionCode _codeSection;

        void addFunction(const ru_function_t& newFunction);
        void headerUpdateOffsets(void);
};





#endif
