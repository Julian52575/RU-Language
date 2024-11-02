#!/bin/bash
EXAMPLES=("multiplePrint")
FAILURES=""
SUCCESS=""
LOG_FILE=""

RED='\033[0;31m'
NC='\033[0m' # No Color

stack build
for current in ${EXAMPLES[@]}; do
    LOG_FILE="examples/log/$current.log"
    stack run "--" "examples/$current.bin" > $LOG_FILE
    ## diff -> FAILURE
    diff $LOG_FILE "examples/$current.txt" > /dev/null
    if [ $? -ne 0 ]
    then
        FAILURE=$FAILURE"\t$current: output differs.\n"
    else
        SUCCESS=$SUCCESS"\t$current: output matches.\n"
    fi
done

echo -e "Succes:\n$SUCCESS"
echo -e "Failures:$RED$FAILURE$NC"
