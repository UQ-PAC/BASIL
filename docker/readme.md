
- asli is compiled from a git checkout of the default branch
- bap 2.5 is compiled from opam 
- basil is compiled from the current staet of this checkout 

The provided compose targets are 

- bap
    - bap with the asli plugin
- basil-build
    - build basil inside a docker container, run this to enter a bash prompt inside the container with the repo mounted
- basil-dev
    - the basil build environment with bap and the asli plugin transplanted into it, used for building into the host 
    environment using the toolchain provided by the container
- basil
    - a smaller container containng just the, bap, basil and cross-compiler binaries

---

Compiling godbolt container

```
podman build -f docker/godbolt.Dockerfile -t ghcr.io/uq-pac/basil-compiler-explorer:latest
podman push ghcr.io/uq-pac/basil-compiler-explorer:latest
```
