
# Run from the directory basil/src/test/*/test_case/compilation_variant/

md5sum-check: a.out $(LIFT_ARTEFACTS)
ifeq ($(USE_DOCKER), 1)
	$(DOCKER_CMD) hash > docker-hash-new
	diff --color -u docker-hash docker-hash-new  # if this fails, make sure your docker image is up-to-date.
	rm docker-hash-new
	md5sum -c md5sums  # using docker; checking compiler output hashes.
else
	echo "not running within docker; skipping docker image validation."
	md5sum -c md5sums
endif

md5sum-update: a.out $(LIFT_ARTEFACTS)
	md5sum $^ > md5sums
	$(ENSURE_DOCKER) $(DOCKER_CMD) hash > docker-hash

$(LIFT_ARTEFACTS): a.out
	$(READELF) -s -r -W a.out > $(NAME).relf
	$(BAP) a.out -d adt:$(NAME).adt -d bir:$(NAME).bir
	$(DDISASM) a.out --ir $(NAME).temp.gtirb
	$(PROTO_JSON) --idem=proto -s8 $(NAME).temp.gtirb $(NAME).gtirb
	rm $(NAME).temp.gtirb
	$(GTIRB_SEMANTICS) $(NAME).gtirb $(NAME).gts

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

.PHONY=recompile verify md5sum-check md5sum-update clean cleanlift cleanall cleanbin cleantest cleangts json gts
verify: $(NAME)_bap.bpl $(NAME)_gtirb.bpl

recompile: a.out

gts: $(NAME).gts

json: $(NAME).gts
	$(DEBUG_GTS) $(NAME).gts > $(NAME).json

$(NAME)_bap_result.txt: $(NAME)_bap.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_bap.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_bap_result.txt"

$(NAME)_gtirb_result.txt: $(NAME)_gtirb.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_gtirb.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_gtirb_result.txt"

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
