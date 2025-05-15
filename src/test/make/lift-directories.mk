.EXPORT_ALL_VARIABLES:

# Run from the directory basil/src/test/*/test_case/

# - lift-directories.mk: sets NAME
#   - config.mk: sets ENABLED_COMPILERS
#   - exec: gcc_pic.mk: sets CC / CFLAGS
#     - lift.mk: sets CONFIG to compiler variant

NAME := $(notdir $(shell pwd))

COMMON_ARTEFACTS := $(NAME).relf
BAP_ARTEFACTS := $(NAME).adt $(NAME).bir
GTIRB_ARTEFACTS := $(NAME).gts

ALL_ARTEFACTS := $(BAP_ARTEFACTS) $(COMMON_ARTEFACTS) $(GTIRB_ARTEFACTS)
# - means continue if it doesnt exist
-include ./config.mk

LIFT_ARTEFACTS ?= $(BAP_ARTEFACTS) $(COMMON_ARTEFACTS) $(GTIRB_ARTEFACTS)

GIT_ROOT?=$(realpath ../../../../)
BUILD_DIR ?= $(shell realpath --relative-to $(GIT_ROOT) .)
MAKE_DIR ?= $(GIT_ROOT)/src/test/make

ifeq ($(USE_DOCKER),1)
	ENSURE_DOCKER := true "using docker" &&
else
	ENSURE_DOCKER := echo "this command should be run within docker" >&2 && false &&
endif

#CFLAGS=-fno-pic -fno-plt
TARGET=aarch64-linux-gnu
GCC ?= aarch64-linux-gnu-gcc
CLANG ?= clang-15 -target $(TARGET)
CC ?= $(GCC)

BAP ?= bap
READELF ?= aarch64-linux-gnu-readelf

DDISASM ?= ddisasm
PROTO_JSON ?= proto-json.py
DEBUG_GTS ?= debug-gts.py
GTIRB_SEMANTICS ?= gtirb-semantics

BASIL=$(GIT_ROOT)/target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar

# paths below are relative to lift.mk's compilation_variant directory.
# note, wildcard is relative to test_case directory.
C_SOURCE ?=$(addprefix ../,$(wildcard *.c))
SPEC ?=$(addprefix ../,$(wildcard *.spec))
EXTRA_SPEC ?=$(addprefix ../,$(wildcard *.bpl))
BASIL_FLAGS ?=
#BOOGIE_FLAGS=/proverOpt:O:smt.array.extensional=false
BOOGIE_FLAGS ?= /useArrayAxioms

ENABLED_COMPILERS ?= clang clang_O2 clang_pic gcc gcc_O2 gcc_pic

TARGETS := all verify repro-stash repro-check md5sum-check md5sum-update clean cleanall cleanlift cleanjson cleangts cleantest recompile json gts
.PHONY : $(TARGETS) $(ENABLED_COMPILERS) docker-start docker-stop

$(TARGETS): $(ENABLED_COMPILERS)

# an empty test case directory may be present for a number of reasons (e.g. git branch switching).
# ignore such directories to avoid more cryptic errors later.
ifeq ($(C_SOURCE),)

# more validity checks
ifneq ($(SPEC)$(EXTRA_SPEC),)
	$(error invalid test case: "$(realpath .)" has .bpl or .spec files but no C file)
endif

$(ENABLED_COMPILERS):
	@echo 'note: skipping directory "$(realpath .)" with no C files...'

else

$(ENABLED_COMPILERS):
	mkdir -p $@/
	$(MAKE) -C $(realpath .)/$@ -f $(MAKE_DIR)/$@.mk $(MAKECMDGOALS)

endif


