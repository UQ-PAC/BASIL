TARGETSS := all verify repro-stash repro-check md5sum-check md5sum-update clean cleanall cleanlift recompile json cleanjson cleangts gts

SUBTARGETS = $(wildcard correct/*/ incorrect/*/)
.PHONY : $(TARGETSS) $(SUBTARGETS) correct incorrect

$(TARGETSS): $(SUBTARGETS)

correct: $(realpath $(wildcard correct/*))
incorrect: $(realpath $(wildcard incorrect/*))

$(SUBTARGETS):
	$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)

# concats md5sums files in subdirectories into a compiled.md5sum.
# check with `md5sum -c compiled.md5sum` in src/test.
.PHONY: compiled.md5sum
compiled.md5sum:
	rm -fv compiled.md5sum
	find correct incorrect -name 'md5sums' -exec bash -c 'cat "$$@" >> compiled.md5sum' '{}' +
