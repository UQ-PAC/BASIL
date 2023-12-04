
$(NAME).bpl: $(LIFT_ARTEFACTS)
	echo $(BASIL)
	java -jar $(BASIL) $(BASIL_FLAGS) --adt $(NAME).adt --relf $(NAME).relf -o $(NAME).bpl --spec $(SPEC)

$(LIFT_ARTEFACTS): a.out
	$(BAP) a.out -d adt:$(NAME).adt -d bir:$(NAME).bir
	$(READELF) -s -r -W a.out > $(NAME).relf

a.out: $(C_SOURCE)
	$(CC) $(CFLAGS) $(C_SOURCE)

.PHONY=verify
verify: $(NAME)_result.txt

$(NAME)_result.txt: $(NAME).bpl $(EXTRA_SPEC)
	boogie $(NAME).bpl $(EXTRA_SPEC) $(BOOGIE_FLAGS) | tee $(NAME)_result.txt

