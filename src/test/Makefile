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
.PHONY: compiled.md5sum extract
compiled.md5sum:
	rm -fv compiled.md5sum
	find correct incorrect -name 'md5sums' -exec bash -c 'cat "$$@" >> compiled.md5sum' '{}' +

compiled.tar.gz: compiled.md5sum
	md5sum --quiet -c compiled.md5sum  # before compressing, make sure our files match expected hashes
	list=`mktemp`; cut -d" " -f3 compiled.md5sum > $$list && tar caf $@ -T $$list && rm $$list

extract:
	curl -z compiled.tar.gz "$$(cat compiled.url.txt)" -o compiled.tar.gz
	tar xf compiled.tar.gz --keep-old-files
	md5sum --quiet -c compiled.md5sum
