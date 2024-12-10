## Writing and Running Tests

See [../../docs/development/readme.md]

## Lifting SystemTest examples

Lifting options specified in make/lift-directories.mk can be overriden using this file, for example to specify the lifting templates:

```
$ cat correct/test_name/config.mk
ENABLED_COMPILERS = clang clang_pic gcc gcc_pic
```

To force recompile and lift all:

```
make cleanall 
make
```

Lift one:

```
                                # relative to correct/secret_write
make -C correct/secret_write -f ../../make/lift-directories.mk
```

or

```
cd correct/secret_write
make -f ../../make/lift-directories.mk
```

### Lifting with Docker

This tests in this repository are a number of source code files which must be
compiled (through gcc and clang) and lifted (by BAP or ddisasm + ASLp)
before they can be processed.
These compiled files can very between compilers, operating systems, and tool versions.
When developing and testing BASIL, it is important that everyone has identical
versions of these files to make sure results are consistent and comparable.

We use a Docker image (built through Nix) as a static
environment for compiling and lifting examples. The Docker image's contained programs
and versions are recorded within this repository, as well as the hashes of generated
files. This ensures everything stays reproducible.

To set up the Docker environment, run this command:
```bash
eval $(make/docker-helper.sh env)
```
This will load a number of environment variables into your shell which are
recognised by the Makefiles and subsequent docker-helper.sh calls.
To de-activate this environment, use the same command with `--unset` after "env".

After setting up the environment, to build and install the Docker image, run:
```bash
docker-helper.sh build
```

To start an instance of the Docker container,
```bash
docker-helper.sh start  # stop with `docker-helper.sh stop`
```

Now, running make commands should use compilers from Docker.
The log of make commands should mention docker or podman
while executing.

To build compiled and lifted files, use `make` as usual.

To check the generated files against the stored hashes, use:
```bash
make md5sum-check
```
To update the hashes after generating new versiosn of the files, use:
```bash
make md5sum-update
```

The Docker image is pinned by make/docker-flake.txt.
To update the Docker image, first make the desired changes
to the pac-nix repository, then update this text file.
After this, you will have to repeat the "env", "build", and "start" subcommands.

Some notes:
- The Docker image will take about 5GB of your disk space.
- The Docker image is an x86_64-linux image. If you are on a different architecture,
  you should configure your Docker to use the x86_64 platform.
- You can run a command within the Docker container with `docker-helper.sh <command>`.
  Note that this will not work with commands needing user interaction (e.g. shells).
- To enter a shell within the Docker container, use `docker-helper.sh shell`.

#### Reproducibility

The Docker container and commands run within it should be reproducible.
If, for any reason, you find unexpected hash mismatches,
you can inspect the file differences between two runs with these commands:

```bash
# after a failed `make md5sum-check`...

make repro-stash  # makes a copy of generated files, then cleans the originals

make repro-check  # re-generates files and diffs them to the stashed copy
```

Note, `make clean` will not remove the stashed copies, but `make cleanall` will.

