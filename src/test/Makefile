TARGETSS := all verify clean cleanall cleanlift recompile json

SUBTARGETS = $(wildcard correct/*/ incorrect/*/)
.PHONY : $(TARGETSS) $(SUBTARGETS) correct incorrect

$(TARGETSS): $(SUBTARGETS)

correct: $(realpath $(wildcard correct/*))
incorrect: $(realpath $(wildcard incorrect/*))

$(SUBTARGETS):
	-$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)
