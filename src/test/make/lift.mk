
# Run from the directory basil/src/test/*/test_case/compilation_variant/

.DELETE_ON_ERROR:

$(LIFT_ARTEFACTS): a.out
	$(READELF) -s -r -W a.out > $(NAME).relf
	$(BAP) a.out -d adt:$(NAME).adt -d bir:$(NAME).bir
	ddisasm a.out --ir $(NAME).temp.gtirb
	proto-json.py --idem=proto -s8 $(NAME).temp.gtirb $(NAME).gtirb
	rm $(NAME).temp.gtirb
	gtirb-semantics $(NAME).gtirb $(NAME).gts
ifeq ($(USE_DOCKER), 1)
	$(DOCKER_CMD) hash > docker-hash-new
	diff -u docker-hash-new docker-hash
	md5sum -c md5sums  # using docker; checking compiler output hashes.
else
	md5sum -c md5sums  # not using docker; ignoring check errors.
endif

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
	$(CC) $(CFLAGS) $(C_SOURCE)

.PHONY=recompile verify md5sums-update clean cleanlift cleanall cleanbin cleantest cleangts json gts
verify: $(NAME)_bap.bpl $(NAME)_gtirb.bpl

recompile: a.out

gts: a.out
	ddisasm a.out --ir $(NAME).gtirb
	gtirb-semantics $(NAME).gtirb $(NAME).gts
	rm -rf $(NAME).gtirb

json:
	debug-gts.py $(NAME).gts > $(NAME).json

$(NAME)_bap_result.txt: $(NAME)_bap.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_bap.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_bap_result.txt"

$(NAME)_gtirb_result.txt: $(NAME)_gtirb.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_gtirb.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_gtirb_result.txt"

md5sums-update: a.out $(LIFT_ARTEFACTS)
	md5sum *.* > md5sums
	$(DOCKER_CMD) hash > docker-hash

cleanall: clean cleanlift cleanbin cleantest cleanjson

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
