#!/bin/bash

CFILE_NAME=""

while getopts "b:c:" o; do
case "${o}" in
    b)
        BIN_NAME=${OPTARG}
        ;;
    c)
        CFILE_NAME="$CFILE_NAME ${OPTARG}"
        ;;
    *) 
        if ${OPTARG} == "--" ; then
        	break;
        fi
    esac
done
shift $((OPTIND-1))

if [[ -z "$CFILE_NAME" ]] ; then 
    echo "Usage: -c cfile.c [-b binaryname] [ -- make args]"
    echo "Optinoally variables CC, CFLAGS, DDISASM, READELF, GTIRBSEM, BAP to change the the binary path to the lifter tools"
    exit 1
fi

BIN_NAME=${BIN_NAME:=$(echo "$CFILE_NAME" | sed -s 's/.c$//' | tr -d ' ')}
export BIN_NAME
export CFILE_NAME

make $@ -f - << 'EOF'
CC ?= aarch64-linux-gnu-gcc
DDISASM ?= ddisasm
READELF ?= readelf
GTIRBSEM ?= gtirb_semantics
BAP ?= bap

.PHONY=all clean 
all: $(BIN_NAME).adt $(BIN_NAME).relf $(BIN_NAME).gts

$(BIN_NAME): $(CFILE_NAME)
	$(CC) $(CFILE_NAME) $(CFLAGS)  -o $(BIN_NAME)

$(BIN_NAME).adt: $(BIN_NAME)
	$(BAP) $(BIN_NAME) -d adt:$(BIN_NAME).adt -d bir:$(BIN_NAME).bir

$(BIN_NAME).relf: $(BIN_NAME)
	$(READELF) -s -r -W $(BIN_NAME) > $(BIN_NAME).relf

$(BIN_NAME).gts: $(BIN_NAME).gtirb
	$(GTIRBSEM) $(BIN_NAME).gtirb $(BIN_NAME).gts

$(BIN_NAME).gtirb: $(BIN_NAME)
	$(DDISASM) $(BIN_NAME) --ir $(BIN_NAME).gtirb

clean:
	rm -f $(BIN_NAME).adt $(BIN_NAME).relf $(BIN_NAME).gts $(BIN_NAME)
EOF

