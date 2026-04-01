{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-mill.url = "github:NixOS/nixpkgs/914a3a6a1024f495af1f5a35c420b170d8b946dd";
    pac-nix.url = "github:katrinafyi/pac-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-mill,
      flake-utils,
      pac-nix,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        millPkgs = import nixpkgs-mill {
          inherit system;
        };
      in
      {
        devShell = pkgs.mkShell {
          LD_LIBRARY_PATH = "${pkgs.gmp}/lib:";

          buildInputs =
            (with pkgs; [
              scala
              scala-cli
              ctags
              metals
              jdk
              boogie
              coursier
              gmp
              # z3
              cvc5
              antlr
              haskellPackages.BNFC

              pkgsCross.aarch64-multiplatform.pkgsBuildHost.gcc
              pkgsCross.aarch64-multiplatform.pkgsBuildHost.clang
              pkgsCross.aarch64-multiplatform.pkgsBuildHost.llvmPackages.clang

              pkgsCross.aarch64-multiplatform-musl.pkgsBuildHost.gcc
              pkgsCross.aarch64-multiplatform-musl.pkgsBuildHost.clang
              pkgsCross.aarch64-multiplatform-musl.pkgsBuildHost.llvmPackages.clang
              sbt

              just

              # bincaml stuff:
              ocaml
              ocamlPackages.ocaml-lsp
              opam
              # gnumake
            ])
            ++ (with millPkgs; [
              # mill
            ])
            ++ [
              pac-nix.packages.${system}.ddisasm
              pac-nix.packages.${system}.gtirb-semantics
              pac-nix.packages.${system}.gtirb-pprinter
              pac-nix.packages.${system}.basil
              pac-nix.packages.${system}.asli
            ];
        };
      }
    );
}
