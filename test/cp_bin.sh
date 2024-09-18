#!/bin/bash

PROJECT=$(eval pwd | sed 's/.*\/\([^/]*\)\/\?$/\1/')
DIR=$(eval stack path --local-install-root)
BIN_PATH=$DIR"/bin/"

cp -r $BIN_PATH ..
