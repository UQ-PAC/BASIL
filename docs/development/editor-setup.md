## Installation and use

This is a Scala 3 project built using the [mill](mill-build.com) build system, it can be run with `mill run`.

### Local Development 

The tool itself is a Scala project and is OS-independent.

Furthermore, lifting input files from a given AArch64 binary is Linux-specific, and all commands given are for Linux. 
On Windows, WSL2 may be used to run any Linux-specific tasks.

Installing a [JDK >= 17](https://openjdk.org/install/) is required.


The mill build tool script can bootstrap the rest of the Scala tool chain by running `./mill build` in the git root.
This can also be installed through [coursier](https://get-coursier.io/docs/cli-overview).

# IDE Support

## VSCode

Mill supports the Metals language server and IntelliJ through Build Server Protocol (BSP), which is the recommended use. 

To use specifically Mill's bsp support, delete `.bsp`, run `./mill mill.bsp.BSP/install`. More information is available [here](https://mill-build.com/mill/Installation_IDE_Support.html#_build_server_protocol_bsp).

When you open the project in VSCode for the first time, it will likely ask you to install Java, and Scala language support extensions.  There is a Scala extension called [`Metals`](https://scalameta.org/metals/docs/editors/vscode/) which may be useful.

## IntelliJ Setup

**This guide assumes that you have java, scala, and ANTLR4 installed on your device. If this is not the case, see the README for instructions on how to install them.**

It's also not necessary to use IntelliJ, other IDEs and text editors can definitely work. However, getting the code to run, and specifically getting the debugger to work is a non-trivial task, so there are instruction here on how to set up everything with IntelliJ.

In the IntelliJ plugin store, download the ANTLR4 and scala plugin.

### Working on the ANTLR4 parser
The ANTLR4 plugin should provide syntax highlighting
for ANTLR4 (.g4) files, as well as code autocompletion and other intellisense features.

The most useful features is the "ANTLR preview" panel found in a tab at the bottom. This allows you input an aribtrary string or file, and look at the visual parse tree or token hierachy. It also allows you to locate text that your grammar is failing to parse, which speeds up debugging.

Note: For large files / complicated parse trees, this can cause a lot of lag and may often crash IntelliJ. You can use the buttons on the left of the ANTLR4 preview pane to disable the visual parse tree generation to reduce this lag (the hierachy tends to be more useful for larger files anyway). By default the parse tree will be automatically regenerated after every edit - this can be disabled in the same plane.
## Working on the Scala code

In order to get IntelliJ to work, we must correctly mark the source directories in this project. Because we have generated source files (i.e. from antlr4), it slightly more involved than a normal project.

### Folders that should be marked as source
- src/main/java
- src/main/scala
### Folders that should be marked as generated source
- target/scala-x.x.x/src_managed/main/antlr4

Compilation should be handled by `mill` using `./mill build`. This will generate the parser source file (from the antlr4 grammar file), 
and then compile all the java and scala source files. 


## Neovim

Some of us use the [AstroNvim](https://astronvim.com/) Neovim distro, for this the community scala support 
[astrocommunity/pack/scala](https://github.com/AstroNvim/astrocommunity/blob/438fdb8c648bc8870bab82e9149cad595ddc7a67/lua/astrocommunity/pack/scala/README.md?plain=1#L2) works out 
of the box if you have coursier installed.

Otherwise the treesitter support and the metals language server can be configured manually.
