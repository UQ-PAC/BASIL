
# Run from the directory basil/src/test/*/test_case/compilation_variant/

$(LIFT_ARTEFACTS): a.out
	$(READELF) -s -r -W a.out > $(NAME).relf
	$(BAP) a.out -d adt:$(NAME).adt -d bir:$(NAME).bir
	ddisasm a.out --ir $(NAME).gtirb
	gtirb-semantics-nix $(NAME).gtirb $(NAME).gts


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

.PHONY=recompile verify clean cleanlift cleanall cleanbin
verify: $(NAME)_bap.bpl $(NAME)_gtirb.bpl

recompile: a.out

$(NAME)bap_result.txt: $(NAME)_bap.bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME)_bap.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_result.txt"

$(NAME)gtirb_result.txt: $(NAME)_gtirb.bpl $(EXTRA_SPEC)
    bash -c "time boogie $(NAME)_gtirb.bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_result.txt"

cleanall: clean cleanlift cleanbin cleantest

cleantest: 
	rm -rf $(NAME).bpl
    rm -rf $(NAME)_bap.bpl
    rm -rf $(NAME)_gtirb.bpl
	rm -rf $(NAME)_result.txt
    rm -rf $(NAME)bap_result.txt
    rm -rf $(NAME)gtirb_result.txt

cleanbin:
	rm -rf a.out
	rm -rf $(NAME).gtirb

clean: cleanlift cleanbin

cleanlift:
	rm -rf $(NAME).adt
	rm -rf $(NAME).bir
	rm -rf $(NAME).relf

