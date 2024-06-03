## Installation and use

This is a Scala 3 project using the [mill](mill-build.com) build system, it can be run with `mill run`. 

### Local Development 

The tool itself is a Scala project and is OS-independent.

Furthermore, lifting input files from a given AArch64 binary is Linux-specific, and all commands given are for Linux. 
On Windows, WSL2 may be used to run any Linux-specific tasks.

Installing [mill](https://mill-build.com/mill/Installation_IDE_Support.html) or [sbt](https://www.scala-sbt.org/download.html) and [JDK >= 17](https://openjdk.org/install/) is required.

This can be done via [coursier](https://get-coursier.io/docs/cli-overview).

A mill script is included at `./mill` so it does not need to be installed. For Windows, `./mill.bat` should be used instead.

#### IDE Support

Mill supports the Metals language server and IntelliJ through Build Server Protocol (BSP), which is the recommended use. 

SBT has an intellij plugin in addition to BSP. 

To use specifically Mill's bsp support, delete `.bsp`, run `mill mill.bsp.BSP/install`, and replace the first argument of `argv` in the resulting `.bsp/mill-bsp.json` with the 
command used to run mill (`mill` if mill is installed, or the mill script, either `./mill` on Linux/OSX or `./mill.bat` on Windows). 

```
~ rm -r .bsp
~ ./scripts/setup-bsp.sh 
# create bsp config file
+ ./mill mill.bsp.BSP/install
[2/2] mill.bsp.BSP.install 
Creating BSP connection file: /home/am/Documents/programming/2023/bil-to-boogie-translator/.bsp/mill-bsp.json
# fix config file 
++ perl -pe 's/"argv":\[".*?",/"argv":["$MILL_CMD",/g' .bsp/mill-bsp.json
+ fixed='{"name":"mill-bsp","argv":["./mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.11.6","bspVersion":"2.1.0-M7","languages":["scala","java"]}'
```
More information is available [here](https://mill-build.com/mill/Installation_IDE_Support.html#_build_server_protocol_bsp).

### Note about using sbt with IntelliJ

[See Also](https://github.com/UQ-PAC/bil-to-boogie-translator/wiki/Development)

The project uses sbt to build the project. For the most part this should *just work*, and there are IntelliJ run files (located in the .run folder) which automatically compile and run the project.
However, to prevent some issues in IntelliJ, it is necessary to unmark `target/scala-3.1.0/src_managed/` as a sources root folder, in the `File > Project Structure > Modules` dialog.

## IntelliJ Setup

**This guide assumes that you have java, scala, sbt and ANTLR4 installed on your device. If this is not the case, see the README for instructions on how to install them.**

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

### Folders that shouldn't be marked as source (or generated source)
- src
- src/main
- target/scala-x.x.x./src_managed/main
- everything else

Compilation should be handled by sbt using `sbt compile`. This will generate the parser source file (from the antlr4 grammar file), and then compile all the java and scala source files. Compiling with IntelliJ will (probably) not generate the parser source file correctly, so compile it using `sbt`.

Running/Debugging can be done through intellij (by running the main method in Main.scala). I am not sure if there is any semantic difference between this and using `sbt run`.

# Getting Started with VSCode

When you open the project for the first time, it will likely ask you to install some Java extensions. There are few dedicated Scala extensions, as the corresponding Java ones also work for Scala anyway. The ones it asks you to download will include the language server, and local `sbt` support (assuming you have `sbt` downloaded already). Once these are downloaded, 

There is a Scala extension called [`Metals`](https://scalameta.org/metals/docs/editors/vscode/) which may be useful.

## Debugging

There is a plugin called "Scala (Metals)" which is the standard for debugging Scala in VSCode. However, we're currently using an outdated version 
of Scala which Metals doesn't directly support anymore, so it has to be configured manually.

Once you've installed Metals, import the (VSCode command prompt: `> Metals: Import Build`). It will then import the project from `build.sbt`. It will also probably say it 
runs into errors trying to import the project - you can ignore these.

Next, VSCode needs to know how to connect to the debugger. Add a new vcsode configuration (in `.VSCode/launch.json`): 

```json
"configurations": [
        {
            "type": "scala",
            "request": "attach",
            "name": "sbt debugger",
            "buildTarget": "root",
            "hostName": "localhost",
            "port": <port>,
        }
    ]
```

Note that the `buildTarget` name is what is found in the `lazy val root` line from `build.sbt`. You can also find it by opening up the Metals plugin's "doctor" (called `Metals Doctor`, started with `> Run doctor`).

To start the debugger, when you open up your sbt terminal (or in the command you use to run the tool), add the flag `-jvm-debug <port>`, e.g.

```bash
$ sbt -jvm-debug <port>
```

Once this has loaded, you can attach the VSCode debugger using the configuration you made earlier (from the run+debug tab, or under Run->Start Debugging). You should then be able to debug like normal in VSCode.

NOTE: if when setting breakpoints you get errors saying "unverified breakpoint", this is because the VSCode debugger can't match the files you're setting them in to locations in the sbt session being debugged. To 
fix this you can try a few things:

- Restarting debugger
- Force Metals to re-import your build
- Change debugger to launch project with debugging flags instead of attaching to a debugger
