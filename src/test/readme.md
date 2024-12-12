# Writing and Running Tests

See [docs/development/readme.md](../../docs/development/readme.md) for context.

This tests in this repository are a number of source code files which must be
compiled (through gcc and clang) and lifted (by BAP or ddisasm + ASLp)
before they can be processed.
This lifting is done deterministically in a Docker container.

The compiled binaries and lifter outputs are not kept inside this repository.
To get started, you can download pre-compiled copies of these files:
```bash
cd src/test
make extract
```
This should be enough to run the SystemTests through mill.

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

6. The last two steps will have to be repeated if the Docker image changes.

### Building with Docker

Now, running make commands should use compilers from Docker.

1. To compile and lift all the examples, use
   ```bash
   cd src/test
   make -j6  # adjust job count as appropriate
   ```
   as usual.
   The log of make commands should mention docker or podman
   while executing. If you see errors about "no container",
   make sure the container is started with the steps above.

   Note that the default `make` rule does not perform any hash checking.
   This is so you can use `make` to compile the files,
   and then re-generate the hashes if needed.

3. Now, we check the produced files against the recorded hashes.
   This makes sure that every file is exactly as expected.
   ```bash
   make md5sum-check -j6
   ```
   Alternatively, you can run `md5sum -c compiled.md5sums`
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
   ```diff
   diff --color -u docker-hash docker-hash-new  # if this fails, make sure your docker image is up-to-date.
   --- docker-hash 2024-12-11 17:11:48.982896545 +1000
   +++ docker-hash-new     2024-12-11 17:12:30.406249598 +1000
   @@ -1,4 +1,4 @@
   -github:katrinafyi/pac-nix/e6b7a676154f08c9d6027d83cd6c9e05fab44145#basil-tools-docker
   +github:katrinafyi/pac-nix/6c430d76555d1723fe2293847653cae18b9af1c9#basil-tools-docker
   ```
   If you see a mismatch in a docker-hash file, your running Docker container does not match
   the one used to generate the files. Make sure you have the right container by repeating
   the "pull" and "start" subcommands of docker-helper.sh.

   If the mismatch is in the md5sum of a compilation output, this likely means the
   compiler is not being deterministic;
   this is a bug and should be reported.
   See the _Troubleshooting_ section below for steps to inspect differences between
   compilation runs.

5. If the md5sum-check succeeds, you are good to go!
   You should repeat these steps if the files have been updated by someone else,
   e.g., to use a more recent version of BAP or ASLp.

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

1. Make your changes in the `src/test/[in]correct/[TESTNAME]` directory.
   A new test directory should have at least a C source file
   and, optionally, a specification file.
   The Makefiles should automatically detect new test cases.

   Lifting options specified in make/lift-directories.mk can be overriden using the config.mk file in each
   test directory. For example, to specify the enabled lifting templates (i.e., compilers and compiler flags):
   ```
   $ cat correct/test_name/config.mk
   ENABLED_COMPILERS = clang clang_pic gcc gcc_pic
   ```

3. Make sure the Docker environment is active with the set up steps.
4. Run `make` to compile and lift your new files.
   If you are only changing a subset of tests, you can limit this using the DIRS or SUBDIRS arguments
   as shown above.
5. Run `make md5sum-update -j6` to generate new hashes.
   Git can be used to compare the differences.
6. In the src/tests directory, run `make compiled.md5sum` to update the combined md5sums file.
7. Create the tarball of generated files with `make compiled.tar.bz2`.
8. Upload compiled.tar.gz to a publicly-accessible file host and update the URL in compiled.url.txt, e.g. by
   ```bash
   curl -Freqtype=fileupload -FfileToUpload=@compiled.tar.bz2 https://catbox.moe/user/api.php
   ```
9. Optionally, check your new hashes are valid and reproducible with `make clean -j4 && make md5sum-check -j4`.
10. Optionally, check the uploaded tarball is valid with `make clean -j4 && make extract`.
11. Commit and PR your changes.

For consistency, you *must* use the Docker environment when
making changes to the hash files with md5sum-update.
The Makefile should make sure that Docker is active.
Of course, do not update any computed md5sum files by hand.

### Updating the Docker image

The Docker image is pinned by make/docker-flake.txt.
This is a string which can be passed to `nix build` to produce the Docker image.
The Docker image is an x86_64-linux image and can only be built on this platform.

To update the Docker image, first make the desired changes
to the pac-nix repository,
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

In the basil repository, it is a good idea to update the docker-flake.txt
and the recorded hashes in the same commit.
This makes sure that at every commit, the test cases can be correctly
built with the corresponding docker-flake.txt.

### Troubleshooting

The Docker container and commands run within it should be reproducible.
If, for any reason, you find unexpected md5sum mismatches,
this is a bug.
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

### Additional notes

- You can run a command within the Docker container with `docker-helper.sh <command>`.
  Note that this will not work with commands needing user interaction (e.g. shells).
- To enter an interactive shell within the Docker container, use `docker-helper.sh shell`.
