# Writing and Running Tests

See [docs/development/readme.md](../../docs/development/readme.md) for context.

This tests in this repository are a number of source code files which must be
compiled (through gcc and clang) and lifted (by BAP or ddisasm + ASLp)
before they can be processed.
This lifting is done deterministically in a Docker container.

<!--

The compiled binaries and lifter outputs are not kept inside this repository.
To get started, you can download pre-compiled copies of these files:
```bash
cd src/test
make clean -j4  # `make extract` will refuse to overwrite existing files
make extract
```
This should be enough to run the SystemTests through mill.
Note that `make extract` will not overwrite any existing output files,
so `make clean` is suggested beforehand.

-->

For much more detail about the lifting process, including how to add or edit
test cases, keep reading.

## Introduction

The directories in src/test are organised by category, then by test case.
For example, `src/test/correct/arrays_simple` is a test case called "arrays_simple"
in the "correct" category (meaning the output is expected to verify through Boogie).
Within each test case folder,
there is a single C source file with the name `[TESTCASE].c`, and
there are subdirectories for each compiler template
("clang", "gcc_O2", etc).

The compiler subdirectories are where the compiler/lifter outputs will go.
These are excluded from the Git repository and their expected hashes
are recorded in the md5sums file.
There is also a ".expected" file which records the Boogie code
produced by BASIL.
The presence or absence of this expected file tells the test infrastructure
whether BASIL is expected to successfully process that test case.


## Lifting reproducibly with Docker

The compiled files can vary between compilers, operating systems, and tool versions.
When developing and testing BASIL, it is important that everyone has identical
versions of these files to make sure results are consistent and comparable.

We use a Docker image (built through Nix) as a static
environment for compiling and lifting examples.
The hashes of generated files, as well as the versions of programs within the Docker image,
are recorded within this repository.
This ensures everything stays reproducible and that everybody has identical copies of
the lifted test cases.

### Setting up Docker
1. Install Podman or Docker through your system's package manager.
2. To set up environment variables, run this command:
   ```bash
   cd src/test
   eval $(make/docker-helper.sh env)
   ```
   This will load a number of environment variables into your shell which are
   recognised by the Makefiles and subsequent docker-helper.sh calls.
   This also adds a `docker-helper.sh` alias into the current shell session.
   To de-activate this environment, use the same command with `--unset` after "env".

   The environment can be configured by setting environment variables before running the "env"
   subcommand. See the "Customising Docker behaviour" subsection below.

3. After setting up the environment, to pull the Docker image [from GHCR](https://github.com/UQ-PAC/BASIL/pkgs/container/basil-tools-docker), run:
   ```bash
   docker-helper.sh pull
   ```
   The Docker image will take about 5GB of your disk space.
   Note that it is an x86_64-linux image.
   If you are on a different architecture,
   you should configure your Docker to virtualise the x86_64 Linux platform.

   (The tag of the image is of the form "flake-HASH-COMMIT"
   where COMMIT is the pac-nix commit where it originates,
   and HASH is a hash of the Nix flake path which produced it.)

5. To start an instance of the Docker image,
   ```bash
   docker-helper.sh start
   ```
   To stop the instance, use the "stop" subcommand.

   Inside the Docker container, the Git repository root will be available at the same file path.
   Files on your computer outside this directory will be unavailable unless manually mounted.

   **If you are on MacOS**, see the section near bottom for important notes.

6. The last two steps will have to be repeated if the Docker image changes.

#### Customising Docker options

Before running `docker-helper.sh env`, several environment variables can be set
to influence its behaviour:

- `DOCKER` is the name of the Docker program (default: podman).
- `DOCKER_USER` is the name/ID of a user _inside_ the container, to be used
  to run the lifter commands (default: root).
- `DOCKER_FLAKE` is a Nix flake reference to a streamed Docker image builder (default: read from docker-flake.txt).
- `DOCKER_IMAGE` is name of the image, including the registry URL (default: ghcr.io/uq-pac/basil-tools-docker)
- `DOCKER_TAG` is the tag of the image (default: hashed from DOCKER_FLAKE).

The values of these variables will be printed by `eval $(docker-helper.sh env)`.
Note that if you want to change these variables, you should first deactivate the environment,
make the changes, then re-activate.
This will make sure dependent variables are updated correctly.
```bash
eval $(docker-helper.sh env --unset)
# set variables here
export DOCKER=docker
# ... etc
eval $(docker-helper.sh env)
```

To use traditional `docker` instead of `podman`:
- By default, the container runner is `podman`. Podman is
  an implementation of the Docker protocol which is rootless by default.
  Containers are run as your usual user, without needing a separate system
  service. Change `DOCKER` to "docker" to use the original Docker program.
- If you are using the traditional `docker` program in a _non_ rootless setup,
  you will have to set `DOCKER_USER` to `UID:GID` where UID/GID are is the user/group ID of
  your _outside_ user (obtain with `id -u` and `id -g`).
  This will make sure the produced files are editable by your ordinary user.

To use a custom Docker image, change `DOCKER_IMAGE` and `DOCKER_TAG` as necessary.
Note that changing these will likely break the "build" and "pull" subcommands, as they
expect a certain format of tags.

### Building with Docker

When the docker-helper.sh environment is active,
running make commands should automatically use the compilers from Docker.

1. To compile and lift all the examples, use
   ```bash
   cd src/test
   make -j6  # adjust job count as appropriate
   ```
   as usual.
   The log of make commands should mention docker or podman
   while executing. If you see errors about "no container",
   make sure the container is started with the steps above.

   Again, **please see the MacOS notes** if relevant.

<!--

   Note that the default `make` rule does not perform any hash checking.
   This is so you can use `make` to compile the files,
   and then re-generate the hashes if needed.

3. Now, we check the produced files against the recorded hashes.
   This makes sure that every file is exactly as expected.
   ```bash
   make md5sum-check -j6
   ```
   Alternatively, you can run `md5sum -c compiled.md5sum`
   to check all files in one batch
   (this will be faster, but the make command is more flexible).

   If the command exits successfully, all the files are valid.

   If it exits with a non-zero code, there will be a message reporting which files have failed
   (the error message may be obscured further up in the output,
   reducing the job count can reduce the noise).
   You should look for messages like these:
   ```bash
   correct/basic_function_call_caller/clang_O2/a.out: FAILED
   md5sum: WARNING: 1 computed checksum did NOT match
   ```
   If the mismatch is in the md5sum of a compilation output, this likely means the
   compiler is not being deterministic;
   this is a bug and should be reported.
   See the _Troubleshooting_ section below for steps to debug the issue and inspect differences between
   compilation runs.

   If you see other errors, such as "file not found", it is possible that there
   are test directories without md5sums, or some other invalid state.

5. If the md5sum-check succeeds, you are good to go!
   You should repeat these steps if the files have been updated by someone else,
   e.g., to use a more recent version of BAP or ASLp.

-->

#### Customising the lifting

Instead of `make` which performs all tests in src/test,
you can restrict it to particular categories or test cases.

To lift one category of tests, use, e.g.,
```bash
make DIRS=correct
```
To lift a single test case (and all its compiler templates), use
```bash
make SUBDIRS=correct/arrays_simple
```
To lift only some compiler templates, use
```bash
make SUBDIRS=correct/arrays_simple ENABLED_COMPILERS=gcc
```
Each of these commands can be suffixed with specific make targets (e.g. "clean", "cleanall").
Multiple terms can be given by quoting them, e.g. `make DIRS='correct incorrect'`
(arguments will be shell-separated).

### Updating or adding test cases
If you change the source code for a test or add a new test case,
you will have to update its hashes as well.
You can use these steps to do so.

1. Make your changes in the `src/test/[CATEGORY]/[TESTNAME]` directory.
   A new test directory should have at least a C source file
   and, optionally, a specification file.
   The Makefiles should automatically detect new test cases. If adding a new category,
   add this to the DIRS variable in the root Makefile.

   Lifting options specified in make/lift-directories.mk can be overriden using the config.mk file in each
   test directory. For example, to specify the enabled lifting templates (i.e., compilers and compiler flags):
   ```console
   $ cat correct/test_name/config.mk
   ENABLED_COMPILERS = clang clang_pic gcc gcc_pic
   ```
   To use only a certain lifter, you can restrict the LIFT\_ARTEFACTS variable to GTIRB\_ARTEFACTS or BAP\_ARTEFACTS.
   ```console
   $ cat src/test/correct/jumptable2/config.mk
   LIFT_ARTEFACTS := $(COMMON_ARTEFACTS) $(GTIRB_ARTEFACTS)
   ```

3. Make sure the Docker environment is active with the earlier set up steps.
4. Run `make` to compile and lift your new files.
   If you are only changing a subset of tests, you can limit this using the DIRS or SUBDIRS arguments
   as shown above.

<!--

5. Run `make md5sum-update -j6` to generate new hashes.
   Git can be used to compare the differences.
6. In the src/tests directory, run `make compiled.md5sum` to update the combined md5sums file.
7. Create the tarball of generated files with `make compiled.tar.zst`.
   Take note of the md5sum line at the bottom.
8. Upload compiled.tar.zst to a publicly-accessible file host and take note of the URL.
   ```bash
   curl -Freqtype=fileupload -FfileToUpload=@compiled.tar.zst https://catbox.moe/user/api.php
   ```
9. Update compiled.url.txt with the new URL and the new md5sum.
10. Optional but recommended, check your new hashes are valid and reproducible with `make clean -j4 && make md5sum-check -j4`.
11. Highly recommended, check the uploaded tarball is valid with
   ```bash
   make clean -j4 && make extract && GCC=/nowhere CLANG=/nowhere make md5sum-check -j6
   ```

-->
12. Commit and PR your changes.

<!--

**Be careful!** If you use `make extract` after editing test cases
but before updating md5sums, the Makefile will assume the extracted
files (from the old test cases) are still valid.
Be sure to make your changes after extracting, or clean the folders
of the edited test cases.

For consistency, you *must* use the Docker environment when
making changes to the hash files with md5sum-update.
The Makefile should make sure that Docker is active.
Of course, do not update any computed md5sum files by hand.

Note that if you do the steps of
(1) make a tarball, (2) extract it, then (3) re-make the tarball, this
_will_ change the md5sum of the tarball (but not its contents).
Be careful when updating compiled.url.txt to make sure the uploaded tarball
matches the md5sum.

-->

### Updating the Docker image

The Docker image is pinned by make/docker-flake.txt.
This is a string which can be passed to `nix build` to produce the Docker image.
The Docker image is an x86_64-linux image and can only be built on this platform.

To update the Docker image, first make the desired changes
to the [pac-nix](https://github.com/katrinafyi/pac-nix) repository,
then update this text file to point to the
relevant commit in pac-nix.
On a x86_64-linux machine, run:
```bash
eval $(make/docker-helper.sh env --reset)
docker-helper.sh build
docker-helper.sh start
```
(If needed, authenticate with Github by `gh auth login --scopes write:packages`.
Then, get the token with `gh auth token`
and use this, along with your Github username,
in `docker login ghcr.io` [docs](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry))

After this,
follow the steps from "updating / adding test cases".
Make sure the changes are as you expect (e.g., if you only updated ASLp, only the ASLp-related
hashes should change).

If this is all fine, push the new Docker to GHCR
```
docker-helper.sh push
```
then commit and push the updated hashes to basil.
Make sure to include the docker-contents.txt
in your Git changes.

In the basil repository, it is a good idea to update the docker-flake.txt
and the recorded hashes in the same commit.
This makes sure that at every commit, the test cases can be correctly
built with the corresponding docker-flake.txt.

<!--

### Troubleshooting

The Docker container and commands run within it should be reproducible.
If, for any reason, you find unexpected md5sum mismatches, follow these steps.

First, try one more time to build the file.
Use these commands:
```bash
make clean SUBDIRS=correct/testcase
make md5sum-check SUBDIRS=correct/testcase
```
Files can be improperly generated in some cases, for example, if a lifter is terminated by Ctrl+C partway through executing.
Of course, do not repeat this too many times
as it may mask non-deterministic behaviour.

If it continues to fail,
this is likely a bug and should be reported.
To aid in debugging,
you can inspect the file differences between two runs with these commands:
```bash
# after a failed `make md5sum-check`...

make repro-stash  # makes a stash of the lifter/compiler outputs, then cleans the directories

make repro-check  # re-generates files and diffs them to the stashed copy
```
If there is non-determinism, the repro-check may coincidentally succeed.
You can use `make clean` to remove generated files but keep the stashed copy,
then repeat repro-check as needed.
Different levels of job count may also affect reproducibility.

-->

### Support on MacOS

Although the tooling is primarily developed on Linux,
efforts are made to keep compatibility with MacOS.
However, there are important to note if you are using a MacOS computer.
If you find additional notes, please add them here.

- You will need **GNU coreutils**. Install with `brew install coreutils`,
  then update PATH to use these by default, e.g.,
  `export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"`.
- You will need **GNU make 4.3+**. Install with `brew install make`,
  then use `gmake` in place of make in all commands.
  If you do not have this, lifting may inexplicably fail
  with "file not found" errors for intermediate files.
- If you see error 137 while making, especially if this happens with high job
  counts,
  you may need to increase the CPUs / memory allocated to your container runner.
  For example with podman,
  ```bash
  podman machine stop
  podman machine set --cpus 6 --memory 8192
  podman machine start
  ```
- The Nix packages aslp, bap, ddisasm, and gtirb-semantics are available
  for the MacOS platform.
  For testing local examples, you can install these native programs then follow the
  "ad-hoc lifting" section below.

### Additional notes

- You can run a command within the Docker container with `docker-helper.sh <command>`.
  Note that this will not work with commands needing user interaction (e.g. shells).
- To enter an interactive shell within the Docker container, use `docker-helper.sh shell`.

## Lifting ad-hoc examples locally

The easiest method to lift an example (without adding it to the test cases)
is to use the `nix-cc.sh` script [described here](https://uq-pac.github.io/BASIL/quick-lifting.html).
This uses a Nix build sandbox to provide some level of isolation and reproducibility,
but this is not as strict as the Docker image.

If you would like to use the lifters/compilers available on your local machine,
you can use the Makefiles without the Docker environment active
(it can be deactivated with `eval $(docker-helper.sh env --unset)`).
This is useful if Docker is especially slow on your computer,
or if you want to test local changes to a lifter.
<!-- However, please make sure to use Docker when updating hashes in the repository. -->

The Makefiles are configured to fall back to programs on your PATH.
These names may be OS-specific or LLVM-version-specific.
You can override these manually by setting some environment variables:
GCC, CLANG, READELF, BAP, DDISASM, PROTO_JSON, DEBUG_GTS, GTIRB_SEMANTICS.

