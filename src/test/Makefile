TARGETSS := all verify repro-stash repro-check md5sum-check md5sum-update clean cleanall cleanlift recompile json cleanjson cleangts gts

# subdirectories of src/test. to be entered into by this makefile.
DIRS := correct incorrect extraspec_correct extraspec_incorrect indirect_calls memory_regions procedure_summaries
# dirs with different directory structure: dsa, irreducible_loops

# non-test dirs: make, scala, unimplemented

# in case the user specfies DIRS, make sure all dirs exist.
$(foreach d, $(DIRS), \
  $(if $(wildcard $(d)/.), \
    , \
    $(error user error: directory "$(d)" in DIRS variable does not exist)))

SUBDIRS = $(wildcard $(addsuffix /*/,$(DIRS)))
.PHONY : $(TARGETSS) $(SUBDIRS) $(DIRS)

# through some unpleasantness, this lets the user specify either DIRS or SUBDIRS
# on the command line, and the make operation will be narrowed to that directory
$(TARGETSS): $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)

# concats md5sums files in subdirectories into a compiled.md5sum.
# check with `md5sum -c compiled.md5sum` in src/test.
.PHONY: compiled.md5sum
compiled.md5sum:
	find $(DIRS) -name '*.md5sum' -exec cat '{}' + | sort -k2 > compiled.md5sum

TARBALL := compiled.tar.zst

$(TARBALL) docker-contents.txt &: compiled.md5sum
	set -u; $$DOCKER_CMD hash > docker-contents.txt  # before compessing, make sure docker-contents.txt is up to date.
	md5sum --quiet -c compiled.md5sum  # before compressing, make sure our files match expected hashes.
	list=`mktemp`; cut -d' ' -f3 compiled.md5sum > $$list && tar caf $(TARBALL) -T $$list && rm $$list
	sha1sum $(TARBALL)

.PHONY: extract
extract:
	# log URL and expected hash
	{ head -n1 compiled.url.txt; tail -n1 compiled.url.txt; } | cat -v
	# check existing file, otherwise download fresh copy.
	{ tail -n1 compiled.url.txt | sha1sum -c - ; } \
		|| curl "$$(head -n1 compiled.url.txt)" -o $(TARBALL)
	# check file type.
	-file $(TARBALL)
	# validate the hash, otherwise remove the incorrect file and abort.
	{ tail -n1 compiled.url.txt | sha1sum -c - ; } || { rm -v $(TARBALL); exit 1; }
	tar xf $(TARBALL) --keep-old-files --touch
	md5sum --quiet -c compiled.md5sum  # check that extracted files match expected checksums

.PHONY: push
push:
	tmp=`mktemp -d` && \
			git clone git@github.com:UQ-PAC/basil-tests.git $$tmp --single-branch --branch basil-src-test && \
			cd $$tmp && \

			rm -rf $$tmp

