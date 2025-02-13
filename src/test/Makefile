TARGETSS := all verify clean cleanall cleanlift recompile json cleanjson cleangts gts

DIRS := correct incorrect analysis-regression 


$(foreach d, $(DIRS), \
  $(if $(wildcard $(d)/.), \
    , \
    $(error user error: directory "$(d)" in DIRS variable does not exist)))


SUBDIRS = $(wildcard $(addsuffix /*/,$(DIRS)))
.PHONY : $(TARGETSS) $(SUBDIRS) $(DIRS)

$(TARGETSS): $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)

