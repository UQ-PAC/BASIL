
# Run from the directory basil/src/test/*/test_case/compilation_variant/

MD5SUM_FILE := $(NAME).md5sum

# compilation variant name. be careful of trailing whitespace!
CONFIG := $(notdir $(realpath .))

# override with config-specific lifter selection
ifdef LIFT_ARTEFACTS_$(CONFIG)
	LIFT_ARTEFACTS := $(LIFT_ARTEFACTS_$(CONFIG))
endif

# $(info $(realpath .) config=$(CONFIG) artefacts=$(LIFT_ARTEFACTS))

all: $(LIFT_ARTEFACTS)

$(NAME).relf: a.out
	$(READELF) -s -r -W a.out > $(NAME).relf
$(NAME).adt $(NAME).bir &: a.out
	$(BAP) a.out -d adt:temp.adt -d bir:temp.bir
	$(MAKE_DIR)/bap-normalise.py temp.adt temp.bir
	mv temp.adt $(NAME).adt && mv temp.bir $(NAME).bir

# if gtirb is missing but required by .gts, do not attempt to build it.
# essentially, missing intermediate files (here, gtirb) will not trigger
# a re-build so long as its products (here, gts) are up-to-date with respect
# to inputs (here, a.out)
.SECONDARY: $(NAME).gtirb
$(NAME).gtirb: a.out
	$(DDISASM) a.out --ir $(NAME).temp.gtirb
	$(PROTO_JSON) --idem=proto -s8 $(NAME).temp.gtirb $(NAME).gtirb  # normalises protobuffer encoding
	rm $(NAME).temp.gtirb
$(NAME).gts: $(NAME).gtirb
	$(GTIRB_SEMANTICS) $(NAME).gtirb $(NAME).gts

repro-stash: $(LIFT_ARTEFACTS)
	rm -rfv repro-stash && mkdir -p repro-stash && mv -v $(LIFT_ARTEFACTS) $(realpath .)/repro-stash

repro-check: $(LIFT_ARTEFACTS)
	[ -d repro-stash ]  # repro-stash must be executed before repro-check
	bash -xeu -c 'cd $(realpath .); for f in $(LIFT_ARTEFACTS); do diff --color -u repro-stash/$$f $$f; done'

BASE_DIR := $(GIT_ROOT)/src/test
RELATIVE_DIR := $(shell realpath --relative-to $(BASE_DIR) .)

md5sum-check: a.out $(LIFT_ARTEFACTS)
ifeq ($(USE_DOCKER), 1)
	# $(DOCKER_CMD) hash > docker-hash-new
	# diff --color -u docker-hash docker-hash-new  # if this fails, make sure your docker image is up-to-date.
	# rm docker-hash-new
	cd $(BASE_DIR) && md5sum -c $(realpath .)/$(MD5SUM_FILE)  # using docker; checking compiler output hashes.
else
	echo "not running within docker; skipping docker image validation."
	cd $(BASE_DIR) && md5sum -c $(realpath .)/$(MD5SUM_FILE)
endif

# paths in md5sum are relative to src/test, to allow for collation into a big md5sums file
md5sum-update: a.out $(LIFT_ARTEFACTS)
	cd $(BASE_DIR) && md5sum $(addprefix $(RELATIVE_DIR)/,$^) > $(RELATIVE_DIR)/$(MD5SUM_FILE) # $^ is all specified dependencies
	# $(ENSURE_DOCKER) $(DOCKER_CMD) hash > docker-hash

ifdef $(SPEC)
BASIL_SPECARG = --spec $(SPEC)
endif

# optional; create basil
$(NAME)_bap.bpl: $(LIFT_ARTEFACTS) $(SPEC) $(BASIL)
	java -jar $(BASIL) $(BASIL_FLAGS) --input $(NAME).adt --relf $(NAME).relf -o $(NAME)_bap.bpl $(BASIL_SPECARG)

$(NAME)_gtirb.bpl: $(LIFT_ARTEFACTS) $(SPEC) $(BASIL)
	java -jar $(BASIL) $(BASIL_FLAGS) --input $(NAME).gts --relf $(NAME).relf -o $(NAME)_gtirb.bpl $(BASIL_SPECARG)

.PHONY=$(BASIL)
$(BASIL):
	cd $(GIT_ROOT) && sbt assembly

# don't re-lift only if binary is missing
.SECONDARY: a.out
a.out: $(C_SOURCE)
	$(CC) $(CFLAGS) '$(C_SOURCE)'

.PHONY=all recompile verify repro-stash repro-check md5sum-check md5sum-update clean cleanlift cleanall cleanbin cleantest cleangts cleanrepro json gts
verify: $(NAME)_bap.bpl $(NAME)_gtirb.bpl

recompile: a.out

gts: $(NAME).gts

$(NAME).json: $(NAME).gts
	$(DEBUG_GTS) $(NAME).gts > $(NAME).json

json: $(NAME).json

$(NAME)_bap_result.txt: $(NAME)_bap.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_bap.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_bap_result.txt"

$(NAME)_gtirb_result.txt: $(NAME)_gtirb.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_gtirb.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_gtirb_result.txt"

cleanall: clean cleanrepro cleantest

cleantest:
	rm -rf $(NAME).bpl
	rm -rf $(NAME)_bap.bpl
	rm -rf $(NAME)_gtirb.bpl
	rm -rf $(NAME)_result.txt
	rm -rf $(NAME)_bap_result.txt
	rm -rf $(NAME)_gtirb_result.txt

cleanbin:
	rm -rf a.out
	rm -rf $(NAME).gtirb

cleanrepro:
	rm -rf repro-stash

clean: cleanlift cleanbin cleanjson

cleanjson:
	rm -rf $(NAME).json

cleanlift:
	rm -rf $(NAME).adt
	rm -rf $(NAME).bir
	rm -rf $(NAME).relf
	rm -rf $(NAME).gts

cleangts:
	rm -rf $(NAME).gts
