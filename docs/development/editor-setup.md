# Getting Started with IntelliJ

**This guide assumes that you have java, scala, sbt and ANTLR4 installed on your device. If this is not the case, see the README for instructions on how to install them.**

It's also not necessary to use IntelliJ, other IDEs and text editors can definitely work. However, getting the code to run, and specifically getting the debugger to work is a non-trivial task, so there are instruction here on how to set up everything with IntelliJ.

In the IntelliJ plugin store, download the ANTLR4 and scala plugin.

## Working on the ANTLR4 parser
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

### Profiling

We can get a flame graph of a profile with linux perf using async profiler

This may require `sudo sysctl kernel.perf_event_paranoid=1`

```
java -agentpath:/path/to/libasyncProfiler.so=start,file=profile.html -jar <basil args>
```
