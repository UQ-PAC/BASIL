# Bil-to-Boogie Translator

## Development 

## Setup 

 1. Download and install [sbt](https://www.scala-sbt.org/download.html) and [boogie](https://github.com/boogie-org/boogie#installation). If you use an operating system with a package manager then the easiest way to install these is likey through your pacage manager (e.g. aur)
 2. Clone the repo

This should be everything. For more details on SBT and getting SBT to work correctly with Intellij see the section below.

### Running a file

To run a single file use the command 

`sbt run file.bil file.elf boogie`

where `file.bil` is the lifted BIL file and `file.elf` is the elf file (for an example of how to generate this, look at `src/scripts/lift\_docker.sh`). A range of sample files can be found in the `samples` folder.

If you are likely to want to run multiple files you can launch the SBT shell using `sbt` and then run each file inside this shell using

`run file.bil file.elf boogie`

This saves the time having to wait for SBT to start.


### Running test suit

To run the test suite run

`sbt test`

This runs a collection of the samples, checking that the correct number of errors are produced and that these occur on the corect line numbers. 

### Manually executing boogie files

It can be helpful to manually run boogie files. For example, to make manual edits to the generated boogie file (`boogie_out.bpl`) for debugging purposes or to run one of the sample output files. To do this run

`boogie boogie.bpl`

where `boogie.bpl` is the name of the boogie file.


### Notes about SBT

The project uses the scala build tool (SBT) to build the project. For the most part this should *just work*, and there are intellij run files (located in the .run folder) which automatically compile and run the project.
However, there are some issues when it comes to using the project in intellij. In particular, it is necassary to go into `target/scala-3.0.0/src_managed` and unmark `main` as a sources root folder. It may also be necassary
to go into a java file and manually enabling `Highlight: All Problems` using the tick in the top right.
Alternatively, these issues do not appear when using the metals LSP (for example when using neovim or vscode).



## Thingss left to do

 - Guarantees
 - Loops

### Tasks for the Summer

 - Value analysis 
 - Points to/alias analysis
 - Reading globals from the symbol table and global offset table
 - Using the call graph
 - Generate the verification conditions
 - Fix jumps (need to properly handle jumps to functions, calls with no return)
 - Replace nulls with option where possible
 - Move over to scala collections
 - Better handling toStrings (there is a lot of overlap between pred/exp and between toString/toBoogieString)
 - Parameter passing (is it possible to reliably detect which registers are used for parameters, particularly when optimisations are on? can we just havoc R0-R7?)

