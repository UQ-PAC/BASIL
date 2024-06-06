# Tool Installation

This describes how to set up a development environment for various development tasks.

We first list the requirements of specific development tasks, and secondly how to obtain them. 

## Tasks

- Run BASIL integration tests
  - Scala, Boogie, Mill
- Lift Integration test examples, add new test case
  - ddisasm, gtirb_semantics, bap, clang-15, gcc-aarch64-linux-gnu, gnu make
- Lift an example binary to gtirb
  - ddisam, gtirb_semantics

## Install instructions

- Scala
    - depends: java 20
    1. [Install coursier](https://get-coursier.io/docs/cli-installation#linux)
    2. `cs install scala`
- Mill
    - depends: Scala
    1. `cs install mill`
    2. `./mill` script in repo root
- Dotnet6
    - usually available from system repositories
    ```
    sudo apt-get install dotnet-sdk-6.0
    ```
    - OR https://dotnet.microsoft.com/en-us/download/dotnet/6.0
- gtirb_semantics 
   - [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage)
    depends: nix
- Boogie
    - depends: Dotnet6, z3
    It is recommended to use Boogie version 3.0.4 and Z3 version 4.8.8 (which is recommended by Boogie). Other versions and combinations may not have been tested.
    ```sh
    dotnet tool install --global Boogie [--version <version>]
    ```
- ddisasm
    - [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage)
    depends: nix
- bap
    -  [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage).
    depends: nix
    - OR use `basil-dev` docker container. depends: podman, podman-compose
    ```
    podman-compose run basil-dev
    ```
    depends: podman, podman-compose
    - Compile from source
- nix
    - Follow the single-user installation instructions [here](https://nixos.org/download/)  
    ```sh
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```
    Follow first-time setup instructions [here](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#first-time) to setup the build cache of PAC packages. 
- java
    - Use system packages `sudo apt install openjdk-20-jdk`
    - Use coursier
    - Use `basil-dev` docker container
- basil-dev docker container
    - `podman-compose run basil-dev`
    - depends: podman, podman-compose
    - [see also](../../docker/readme.md)
- Podman / docker
    - Available from package repositories
    ```
    sudo apt-get install podman podman-compose
    ```
- clang
    - available from package repositories
- gcc cross
    - Available from system repositories `gcc-aarch64-linux-gnu`
- z3
    - available from system repositories
