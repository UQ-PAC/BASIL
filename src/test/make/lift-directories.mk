.EXPORT_ALL_VARIABLES:

# Run from the directory basil/src/test/*/test_case/

# - means continue if it doesnt exist
-include ./config.mk

NAME=$(notdir $(shell pwd))
GIT_ROOT?=$(realpath ../../../../)

GCC ?= aarch64-linux-gnu-gcc
CLANG ?= clang-15 -target $(TARGET)
CC ?= $(GCC)
#CFLAGS=-fno-pic -fno-plt
TARGET=aarch64-linux-gnu

BAP?=bap
READELF ?= aarch64-linux-gnu-readelf
BASIL=$(GIT_ROOT)/target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar

C_SOURCE ?=$(realpath $(wildcard *.c))
SPEC ?=$(realpath $(wildcard *.spec))
EXTRA_SPEC ?=$(realpath $(wildcard *.bpl))
BASIL_FLAGS ?= 
#BOOGIE_FLAGS=/proverOpt:O:smt.array.extensional=false
BOOGIE_FLAGS ?= /useArrayAxioms

LIFT_ARTEFACTS=$(NAME).adt $(NAME).bir $(NAME).relf $(NAME).gts

ENABLED_COMPILERS ?= clang clang_O2 clang_pic clang_no_plt_no_pic gcc gcc_O2 gcc_no_plt_no_pic gcc_pic

TARGETS := all verify clean cleanall cleanlift recompile
.PHONY : $(TARGETS) $(ENABLED_COMPILERS)

$(TARGETS): $(ENABLED_COMPILERS)

$(ENABLED_COMPILERS):
	mkdir -p $@/
	# - continue if fails
	-$(MAKE) -C $(realpath $@) -f $(GIT_ROOT)/src/test/make/$@.mk $(MAKECMDGOALS)

