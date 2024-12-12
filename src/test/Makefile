TARGETSS := all verify repro-stash repro-check md5sum-check md5sum-update clean cleanall cleanlift recompile json cleanjson cleangts gts

# subdirectories of src/test. to be entered by this makefile
DIRS := correct incorrect analysis extraspec_correct extraspec_incorrect indirect_calls memory_regions procedure_summaries
# dirs with irregular structure: dsa, irreducible_loops
# non-test dirs: make, scale, unimplemented

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
.PHONY: compiled.md5sum extract
compiled.md5sum:
	find $(DIRS) -name 'md5sums' -exec cat '{}' + > compiled.md5sum

compiled.tar.gz docker-contents.txt &: compiled.md5sum
	set -u; $$DOCKER_CMD hash > docker-contents.txt  # before compessing, make sure docker-contents.txt is up to date.
	md5sum --quiet -c compiled.md5sum  # before compressing, make sure our files match expected hashes.
	list=`mktemp`; cut -d" " -f3 compiled.md5sum > $$list && tar caf $@ -T $$list && rm $$list

extract:
	curl -z compiled.tar.gz "$$(cat compiled.url.txt)" -o compiled.tar.gz
	tar xf compiled.tar.gz --keep-old-files
	md5sum --quiet -c compiled.md5sum
