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

a.out: $(C_SOURCE)
	$(CC) $(CFLAGS) $(C_SOURCE)

.PHONY=verify clean cleanlift cleanall
verify: $(NAME)_result.txt 

$(NAME)_result.txt: $(NAME).bpl $(EXTRA_SPEC)
	bash -c "time boogie $(NAME).bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_result.txt"

cleanall: clean cleanlift

clean: 
	rm -rf $(NAME).bpl
	rm -rf $(NAME)_result.txt

cleanlift:
	rm -rf $(NAME).adt
	rm -rf $(NAME).bir
	rm -rf $(NAME).relf
	rm -rf a.out

