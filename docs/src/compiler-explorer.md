# Compiler-Explorer

(Note this has not been supported for a while)

Point of contact: @ailrst

This includes explanation of the [Godbolt / compiler-explorer](compiler-explorer.com) wrapper for the basil tool.

The tool roughly keeps track with the version of BASIL on the main branch, but updating is a manual process.

There is also instructions in the GitHub on running the container locally, this would allow you to mount a different 
version of basil into the container. 

```
podman run  -p 10240:10240 ghcr.io/uq-pac/basil-compiler-explorer:latest
```

#### Using the tool

1. When you visit the page it will ask for a GitHub login since it only allows access if you are a member of the UQ-PAC GitHub org. 
    - If you are trying to follow a link to a specific example the query params for the example get dropped by the login page so you will have to click the link again. 
2. It defaults to a C++ template which isn't configured with any of the tools, go to templates in the top left and choose the "Secret Write Spec" template. 
    - This has a C file and a spec file you can edit to test. 

The way compiler-explorer works is with a simple pipeline 

    Source Tree   ->    Compiler     ->     Tool

    example.c           gcc/clang           basil etc
    example.spec          
        |                  |                    |
        v                  v                    v
     Editor              Assembly             Tool 
     windows             Window               Windows


- This means every tool belongs to a compiler/assembly window, 
and every compiler/assembly window belongs to a source tree or source 
window. 
    - This is indicated in the default pane names
    - You can have multiple pipelines run at the same time in different windows, 
      eg. two different compilers on the same source.


3. To view other tool outputs; go to the Tools dropdown in the assembly output window to add another tool 
  parented to this compiler/assembler. 

![](img/godboltexample.png)

The notable tools are:

- `basil` - run bap, readelf and basil and output the boogie file 
- `BAP (BIR)` - run bap on the binary and output the bir file
- `BAP (ADT)` - run bap on the binary and output the adt file. 
- `readelf (aarch64)`  - output of `aarch64-linux-gnu-readelf -s -r -W`
- `boogie` - run readelf, bap, basil and boogie on the result

#### Tool Arguments

- If you modify the arguments for a tool, compiler, or the source code the entire pipeline will be re-run. No window is updated until every tool is finished, 
  this can be a long time if you are running boogie. 
- Note that while tools are parented to a compiler and source window, they do not talk to each other. 
  They also don't automatically know to use a spec file if it is available, so **the spec file must be 
  explicitly set on each window that uses it (basil or boogie)**. 
- Arguments can be explicitly passed to the underlying tool by adding `--args 'arg1 arg1 ...'` to the Arguments field in the interface.  
    (Single quotes are important)
- You can see the output of previous stages in a specific tool window using the `--output/-o` argument. E.g. `-o basil-il` to see Basil IL
    in the basil or boogie tools. The outputs available in the basil tool for example are `boogie, basil-il, adt, bir, relf`. 
- If you revert a source or argument change to exactly the same as a previous state a cached output may be used. 

![](img/args.png)

#### Compiler Arguments

- You can set compiler arguments in the assembly window, for example `-O2`.  
- The tool does not work well unless the binary is compiled **and linked**. 
  You must have `Link to binary` checked in the output menu for it to work. 

![](img/compileropts.png)


#### Running Boogie Directly

1. Open a new source editor (Add → Source Editor)
2. Change the language to "Boogie IVL"
3. Add boogie code to the window
4. In the source editor, add a compiler (+ Add new → Compiler) 
5. To pass extra arguments to boogie you need to pass then through by putting `--args '/arg1 /arg2'` in the Arguments
   field in the compiler window. 

#### Sharing/Saving Examples

The share button in the top right allows sharing examples. "Short links" aren't guaranteed to live forever, but the "Full Link" 
encodes the example in the actual url, so should be pretty reliable unless the URL is very large. 

The Tree window also allows you to save or import the tree as a zip.

