TARGETSS := all verify clean cleanall cleanlift recompile json cleanjson cleangts gts

SUBTARGETS = $(wildcard correct/*/ incorrect/*/ correct_analyse/*/)
.PHONY : $(TARGETSS) $(SUBTARGETS) correct incorrect

$(TARGETSS): $(SUBTARGETS)

correct: $(realpath $(wildcard correct/*))
incorrect: $(realpath $(wildcard incorrect/*))

$(SUBTARGETS):
	-$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)
