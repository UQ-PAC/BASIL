
# Run from the directory basil/src/test/*/test_case/compilation_variant/

$(LIFT_ARTEFACTS): a.out
	$(BAP) a.out -d adt:$(NAME).adt -d bir:$(NAME).bir
	$(READELF) -s -r -W a.out > $(NAME).relf

# optional; create basil
$(NAME).bpl: $(LIFT_ARTEFACTS) $(SPEC) $(BASIL)
	echo $(BASIL)
	java -jar $(BASIL) $(BASIL_FLAGS) --adt $(NAME).adt --relf $(NAME).relf -o $(NAME).bpl --spec $(SPEC)

.PHONY=$(BASIL)
$(BASIL): 
	cd $(GIT_ROOT) && sbt assembly

# don't re-lift only if binary is missing 
.SECONDARY: a.out
a.out: $(C_SOURCE)
	$(CC) $(CFLAGS) $(C_SOURCE)

.PHONY=recompile verify clean cleanlift cleanall cleanbin
verify: $(NAME)_result.txt 

recompile: a.out

$(NAME)_result.txt: $(NAME).bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME).bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_result.txt"

cleanall: clean cleanlift cleanbin

cleanbin:
	rm -rf a.out

clean: cleanbin
	rm -rf $(NAME).bpl
	rm -rf $(NAME)_result.txt

cleanlift:
	rm -rf $(NAME).adt
	rm -rf $(NAME).bir
	rm -rf $(NAME).relf

