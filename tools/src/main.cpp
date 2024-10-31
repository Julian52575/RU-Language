#include "instructionFactory.hpp"
#include "ru.h"
#include "ru.hpp"

static void
worker(ruFile& ru, instructionFactory& facto)
{
    ruInstruction worker;

    worker = facto.createVar(RU_VARTYPE_INT, 0xaaaaaaaa);
    ru.addInstruction(worker);
    worker = facto.createVar(RU_VARTYPE_INT, 0xbbbbbbbb);
    ru.addInstruction(worker);
    //function1
    worker = facto.createVar(RU_VARTYPE_INT, 0xcccccccc);
    worker.setFunctionStart("function1");
    ru.addInstruction(worker);
    worker = facto.createVar(RU_VARTYPE_INT, 0xdddddddd);
    ru.addInstruction(worker);
    worker = facto.createVar(RU_VARTYPE_INT, 0xeeeeeeee);
    ru.addInstruction(worker);
    //function2
    worker = facto.createVar(RU_VARTYPE_INT, 0xffffffff);
    worker.setFunctionStart("function2");
    ru.addInstruction(worker);
    worker = facto.createVar(RU_VARTYPE_INT, 0xaaaaaaaa);
    ru.addInstruction(worker);
    worker = facto.createVar(RU_VARTYPE_INT, 0xbbbbbbbb);
    ru.addInstruction(worker);
}

int main(int ac, char * const * av)
{
    ruFile ru;
    instructionFactory facto;

    if (ac > 1) {
        ru.setFileName(av[1]);
    }
    worker(ru, facto);
    ru.writeToFile();
    return 0;
}
