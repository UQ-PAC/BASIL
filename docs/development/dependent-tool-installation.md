# Tool Installation

This describes how to setup a development environment for various development tasks.

We first list the requirements of specific development tasks, and secondly how to obtain them. 

## Tasks

- Run BASIL integration tests
  - Scala, Boogie, Mill
- Lift Integration test examples, add new test case
  - ddisasm, gtirb_semantics, bap, clang-15, gcc-aarch64-linux-gnu
- Lift an example binary to gtirb
  - ddisam, gtirb_semantics

## Install instructions

- Scala
    1. [Install coursier](https://get-coursier.io/docs/cli-installation#linux)
    2. `cs install scala`
    - depends: java 20
- Mill
    - `cs install mill`
    - `./mill` script in repo root
    - depends: Scala
- Dotnet6
    - usually available from system repositories
    ```
    sudo apt-get install dotnet-sdk-6.0
    ```
    - https://dotnet.microsoft.com/en-us/download/dotnet/6.0
- gtirb_semantics 
   - [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage)
    depends: nix
- Boogie
    - depends: Dotnet6
    ```sh
    dotnet tool install --global Boogie [--version <version>]
    ```
- ddisasm
    - [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage)
    depends: nix
- bap
    -  [use nix derivation](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#usage).
    depends: nix
    - Use `basil-dev` docker container.
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
    - `sudo apt-get install podman podman-compose`
- clang
  - available from package repositories
- gcc cross
  - Available from system repositories `gcc-aarch64-linux-gnu`









