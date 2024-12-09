
SOURCES=$(wildcard *.c)
SOURCENAMES=$(basename $(SOURCES))
RELFS=$(addsuffix .relf,$(SOURCENAMES))
ADTS=$(addsuffix .adt,$(SOURCENAMES))
OUTS=$(addsuffix .out,$(SOURCENAMES))
CHECKSUMS=$(addsuffix .checksum,$(SOURCENAMES))

CSMITH_HEADER ?= "/usr/include/csmith/"
CC=clang -target aarch64-linux-gnu

%.out : %.c
	$(CC) $< -w -I $(CSMITH_HEADER) -o $@

%.relf : %.out
	readelf -s -r -w $< > $@

%.adt: %.out
	bap $< -d adt:$@

%.checksum : %.c
	clang -w -I $(CSMITH_HEADER) $< -o $<.out && ./$<.out || true

default : $(ADTS) $(RELFS)

.PHONY=checksum
checksum: $(CHECKSUMS)

clean:
	rm -f *.out *.relf *.adt *.bir *.checksum

