TARGETSS := all verify md5sum-check md5sum-update clean cleanall cleanlift recompile json cleanjson cleangts gts

SUBTARGETS = $(wildcard correct/*/ incorrect/*/)
.PHONY : $(TARGETSS) $(SUBTARGETS) correct incorrect

$(TARGETSS): $(SUBTARGETS)

correct: $(realpath $(wildcard correct/*))
incorrect: $(realpath $(wildcard incorrect/*))

$(SUBTARGETS):
	$(MAKE) -C $@ -f $(realpath ./make/lift-directories.mk) $(MAKECMDGOALS)


DOCKER_FLAKE := $(shell cat make/docker-flake.txt)
.PHONY: docker-build docker-start docker-stop

docker-build: docker-stop
	DOCKER_FLAKE=$(DOCKER_FLAKE) make/docker-helper.sh build

docker-start: docker-stop
	make/docker-helper.sh start

docker-stop:
	make/docker-helper.sh stop
