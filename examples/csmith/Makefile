
SOURCES=$(wildcard *.c)
SOURCENAMES=$(basename $(SOURCES))
RELFS=$(addsuffix .relf,$(SOURCENAMES))
ADTS=$(addsuffix .adt,$(SOURCENAMES))
OUTS=$(addsuffix .out,$(SOURCENAMES))
CHECKSUMS=$(addsuffix .checksum,$(SOURCENAMES))

CSMITH_HEADER = "/nix/store/6rawnpny818v7wki6zlr3i0f3phmysyb-csmith-2.3.0/include/csmith-2.3.0/"

%.out : %.c
	aarch64-suse-linux-gcc $< -w -I $(CSMITH_HEADER) -o $@


%.relf : %.out
	aarch64-suse-linux-readelf -s -r -W $< > $@

%.adt: %.out
	bap $< -d adt:$@

%.checksum : %.c
	clang -w -I $(CSMITH_HEADER) $< -o $<.out && ./$<.out || true

default : $(ADTS) $(RELFS)


.PHONY=checksum
checksum: $(CHECKSUMS)

clean:
	rm -f *.out *.relf *.adt *.bir *.checksum

