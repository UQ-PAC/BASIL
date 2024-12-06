.EXPORT_ALL_VARIABLES:

# Run from the directory basil/src/test/*/test_case/

# - means continue if it doesnt exist
-include ./config.mk

NAME=$(notdir $(shell pwd))
GIT_ROOT?=$(realpath ../../../../)
BUILD_DIR ?= $(shell realpath --relative-to $(GIT_ROOT) .)
MAKE_DIR ?= $(GIT_ROOT)/src/test/make

ENSURE_DOCKER := [[ "$(USE_DOCKER)" == 1 ]] &&

GCC ?= aarch64-linux-gnu-gcc
CLANG ?= clang-15 -target $(TARGET)
CC ?= $(GCC)
#CFLAGS=-fno-pic -fno-plt
TARGET=aarch64-linux-gnu

BAP?=bap
READELF ?= aarch64-linux-gnu-readelf

BASIL=$(GIT_ROOT)/target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar

# paths below are relative to lift.mk's compilation_variant directory
C_SOURCE ?=$(addprefix ../,$(wildcard *.c))
SPEC ?=$(addprefix ../,$(wildcard *.spec))
EXTRA_SPEC ?=$(addprefix ../,$(wildcard *.bpl))
BASIL_FLAGS ?= 
#BOOGIE_FLAGS=/proverOpt:O:smt.array.extensional=false
BOOGIE_FLAGS ?= /useArrayAxioms

LIFT_ARTEFACTS=$(NAME).adt $(NAME).bir $(NAME).relf $(NAME).gts

ENABLED_COMPILERS ?= clang clang_O2 clang_pic gcc gcc_O2 gcc_pic

TARGETS := all verify md5sum-check md5sum-update clean cleanall cleanlift cleanjson cleangts cleantest recompile json gts
.PHONY : $(TARGETS) $(ENABLED_COMPILERS) docker-start docker-stop

$(TARGETS): $(ENABLED_COMPILERS)

$(ENABLED_COMPILERS):
	mkdir -p $@/
	# - continue if fails
	$(MAKE) -C $(realpath $@) -f $(MAKE_DIR)/$@.mk $(MAKECMDGOALS)


