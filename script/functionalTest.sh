#!/bin/bash
INPUT="script/default.ru"
OUTPUT="script/output.izly"


stack run -- $INPUT $OUTPUT

if ! [ -f $OUTPUT ]
then
    echo "$OUTPUT not found."
    exit 84
fi
exit 0
