

BIN ?= a.out
DDISASM ?= ddisasm
BAP ?= bap
GTIRB_SEM ?= gtirb-semantics
READELF ?= aarch64-unknown-linux-gnu-readelf

all: $(BIN).gts $(BIN).adt $(BIN).relf

bap: $(BIN).adt $(BIN).relf
gtirb: $(BIN).gts $(BIN).relf


$(BIN).gtirb : $(BIN)
	$(DDISASM) $(BIN) --ir $(BIN).gtirb

$(BIN).gts : $(BIN).gtirb
	$(GTIRB_SEM) $(BIN).gtirb $(BIN).gts

$(BIN).adt : $(BIN)
	$(BAP) $(BIN) -d adt:$(BIN).adt -d bil:$(BIN).bil

$(BIN).relf : $(BIN)
	$(READELF) -s -r -W $(BIN) > $(BIN).relf

clean: 
	rm -f $(BIN).relf $(BIN).adt $(BIN).bil $(BIN).gts $(BIN).gtirb
