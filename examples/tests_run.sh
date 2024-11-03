#!/bin/bash
EXAMPLES=("multiplePrint" "returnString" "function" "fibonacci" "for")
FAILURES=""
SUCCESS=""
LOG_FILE=""

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

rm examples/log/*.log
stack build
for current in ${EXAMPLES[@]}; do
    if ! [ -f "examples/$current.txt" ]
    then
        FAILURES=$FAILURES"\texamples/$current.txt not found."
        continue
    fi

    LOG_FILE="examples/log/$current.log"
    stack run "--" "examples/$current.bin" > $LOG_FILE
    ## diff -> FAILURE
    diff $LOG_FILE "examples/$current.txt" > /dev/null
    if [ $? -ne 0 ]
    then
        FAILURES=$FAILURES"\t$current: output differs.\n"
    else
        SUCCESS=$SUCCESS"\t$current: output matches.\n"
    fi
done

echo -e "Succes:\n$GREEN $SUCCESS $NC"
echo -e "Failures:\n$RED $FAILURES $NC"

if [[ $FAILURES == "" ]]
then
    exit 0
else
    exit 84
fi
