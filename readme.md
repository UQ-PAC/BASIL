# To the Summer Students: Introduction to the Boogie Translator

By James Tobler

## Introduction

As you may know, a central part of our program verification tool is translating BIL code to verifiable Boogie code. As a starting point, I built a partially-implemented solution over the course of approximately 24 working days, during last semester. It is now at the point where it could greatly benefit from a few more hands, and that’s where you come in. In this informal document, I’ll detail our vision for the program, what has been done so far, what you can do to contribute and tips for building on the current codebase.

## Data Structures

### Fact

To translate BIL to Boogie, we need some object that can store information about each line in the BIL code, be manipulated by the translation tool and finally be output as Boogie code. This data structure is called a fact, named after datalog facts, as it stores information in a similar manner.  
In the tool, you can create a fact representing any BIL instruction (i.e. ‘line’) or expression (i.e. an operation within a line), such as assignments (an instruction), function calls (an instruction), binary operations (an expression of the form (variable 1) (operator) (variable 2), like ‘x + y’), empty lines and much more. However, there are some exceptions. Currently, one exception is BIL cast expressions, which add or remove padded 0’s around variables which need to be converted to a different size in memory. We don’t need these casts in Boogie, so we don’t create facts for them.  
Facts can contain other facts, referred to as children. For instance, the aforementioned binary operator fact contains two children: the first variable (represented as a variable fact) and the second variable. Since they are designed to be highly malleable, they also come with a lot of useful methods, such as recursively adding their children into a list, or replacing one of their children if it matches a fact passed in as an argument.  
Each fact overrides the toString() method, which outputs valid Boogie code representing the fact.  
Facts fall into one of two abstract types: instruction facts and expression facts.

#### Instruction Fact

In general, instruction facts represent a ‘line’ of BIL code (which includes function headers). As such, each instruction fact contains a unique ‘label’, which can be made visible or hidden.  
Unlike expression facts, instruction facts do not override the equals method, as it is desirable to consider each line to be unique, even if they contain the same contents. If we did want to override the equals method, the override would have to ignore labels.

#### Expression Fact

In general, expression facts represent some kind of operation, variable or value. Every expression fact is contained by some instruction fact, and expression facts can contain other expression facts, depending on the type of expression.  
Expression facts do override the equals method, as it is desirable to detect identical expressions. An example case would be where we want to replace all instances of the binary expression “2 + 3” with the literal expression “5”. This requires identifying that “2 + 3”.equals(“2 + 3”) == true (where “2 + 3” represents the expression fact).

### Flow Graph

#### Block

All flow graphs are fundamentally made of blocks. These blocks represent ‘basic blocks’ in Boogie, where each block consists of some label, and a list of lines (in our case, facts). Blocks are nodes in the flow graph structure, and as such, they link to other ‘children’ blocks by storing them in a list. In this way, a block can return all the blocks it can recursively reach by conducting a depth-first search on its children.  
A block begins with any line that is: the first line in the BIL file, a function header, a line that is jumped to by another line or a line that directly follows the last line of a block (except if this following line is a function header). A block ends with any line that is: the last line in the BIL file, a function return statement, a jump instruction or any line where the following line is a function header.  
One must be careful when attempting to modify the list of lines within a block. For instance, modifying the list returned by the method getLines() will successfully modify the block, but modifying the list returned by the method getLinesInCluster(), which returns a combined list of all lines of all blocks reachable by this block, will not succeed, as the returned list is not the one stored by the block. This is probably poor design and will be changed in future, but heed this warning.  
Example Block.toString() output:
```
my_block:
    X0 := 0;
    X0 := X0 + 1;
    X1 := X0;
```

#### Function

As far as I understand, all BIL code runs within a function, starting with main. As such, it is useful to model the BIL code as a flow graph which contains a collection of functions, where each function contains a collection of blocks. More details about these relationships is under “Flow Graph”, but for now, you should just know that functions contain two variables.  
First, they contain the header fact of the function they represent. This is an instruction fact that contains the name of the function, a list of parameters and other useful metadata about the function itself.  
Second, they contain the ‘root block’ of the function. This is the first block that is executed when the function is called (essentially, the lines just below the function header). The root block can provide access to any block within the function by conducting a depth-first-search on its children.  
Example Function.toString() output:
```
procedure my_func(param1, param2) {
    my_block:
        X0 := 0;
        X0 := X0 + 1;
        X1 := X0;
    a_second_block:
        X1 := X1 + 1;
    another_block:
        if (X1 == 2) goto a_second_block;
        return;
}
```

#### Flow Graph

The flow graph is the central data structure that translation occurs on. It is essentially a collection of functions, which are in-turn directed graphs of blocks. The flow graph provides us with all the possible paths an execution thread can take on any given function within the BIL file. Note that there are no edges between blocks of different functions (function calls, unlike jumps, are not considered a ‘linking’ instruction). Flow graphs can be visualised as follows:

<img width="484" alt="image" src="https://user-images.githubusercontent.com/64625414/142977928-5e961d04-919f-4115-82c0-74e09b54cbd0.png">

## The Translation Process

### A High-Level Overview

Suppose we have just lifted our assembly into a fresh new BIL file. To translate this to Boogie, we perform the following general steps, which will be expanded on next:
1. Stripping the BIL file of boilerplate, and other preprocessing.
2. Convert the BIL file into a list of internally-stored fact objects.
3. Convert the list of facts into a flow graph.
4. Perform analysis and modification on the flow graph to create verifiable Boogie code.
5. Write the flow graph to a file.

![image](https://user-images.githubusercontent.com/64625414/143155979-74622034-0373-4623-884f-0fe0f16d4ee6.png)

### Preprocessing

#### Stripping the BIL file

Our fresh new BIL file is going to have a lot of boilerplate code that we don’t want to include in our Boogie translation. Unfortunately, automatically stripping this boilerplate is quite a challenge, as there are no consistent indicators of where the boilerplate ends and begins. The process of identifying the useful parts of the BIL file may have to look something like this:
1. Find the main function.
2. Perform flow control analysis from the beginning of the main function to find all parts of the BIL file that can be accessed from main, including other functions that are called from within main.
3. Collect all the traversed BIL lines into a list (or flow graph) and return.

Hence it may be useful to perform this stripping simultaneously with flow graph creation.

#### Collecting Global Variables

As far as I’ve seen, global lines of code do not show up in the BIL file (or if they do, they may be somewhere quite obscure). Furthermore, BIL does not refer to variables by name, but by memory address. Hence, in order to analyse and translate global variables, it is necessary to perform some analysis on the compiler symbol table in order to identify global variables and their memory addresses.

### Parsing BIL into facts

We use a tool called Antlr to create an abstract syntax tree (AST) of the BIL file (relevant Antlr files are in the BilParsing package) and use a custom-built listener called StatementLoader (found in the BILTranslating package) to convert the AST into a list of facts.  
Statement loader also performs some special operations, such as splitting a BIL call line (which internally carries a goto after the call: e.g. “call func() return %0000031b”) into two facts - a call fact and a jump (i.e. goto) fact (e.g. we’ll have “call func()” and then also “goto %0000031b” directly following, as two separate facts).

### Creating a flow graph

The process of creating a flow graph from a list of facts is…tedious. The following is a general outline of the steps taken. Flow graphs are created by the FlowGraph.FlowGraphFactory class.

#### Splitting

To start, we want to identify where we want to break the BIL file into blocks. We do this by going through the file, from top to bottom, and adding “splits” at each point where a block should begin or end. Rules for splits are explained in the Block section under Data Structures. We now have a list of indexes telling us where to end a block and start another.

#### Creating Blocks

For each segment between two splits, we create a block consisting of that segment.

#### Setting Children

We now want to link each block to its children. We do this by analysing the jumps and conditional jumps at the end of each block, then finding the blocks which begin with those lines that are jumped to, then finally adding those blocks as children of the block that is jumped from.

#### Converting Blocks to Functions

We now have a collection of interconnected blocks. To create functions from this collection, we find all the blocks which begin with function header facts, and create a function for each of them, setting these blocks as the root blocks for our functions. We can access all other blocks in the function by performing a depth-first search on the children of the root block.

#### Creating the Flow Graph

Finally, we create a flow graph consisting of our list of functions, and we are done!

### Analysis and Modification

Once the flow graph has been created, it needs to be analysed and modified in order to be converted to valid Boogie code. All of this is done in the BoogieTranslator class in the BilTranslating package. In this class, you’ll find a translate() method that consists of a list of method calls. Each of these calls is a ‘layer’ of translation. After applying these layers in order, we expect the flow graph to be appropriately modified. Some layers depend on others, which can be specified in the Javadoc of that layer’s method. Though we intend to add more layers later (and fix some that are already there!), below is a brief description of what each layer currently does.

#### initGlobalBlock()

Initialise the global block. When we created our flow graph, we gave it a list of Functions. We also gave it an empty global block, which is a single block that exists outside of functions, and is the starting point for our Boogie program. Right now, this layer simply inserts a call to main() in the global block, though it is here that we intend to add the initialisation of global variables later on.

#### createLabels()

Create labels. When we created our list of facts through StatementLoader, each instruction fact was given a label. This label can be set to be visible or hidden, and is by default hidden. This layer detects which labels must be shown (for instance, because they are referred to by other facts, such as jump facts), and sets them to be visible.

#### optimiseSkips()

Sometimes BIL will output an empty instruction (i.e. a blank line). The facts that StatementLoader converts these to are called NopFacts (“no operation” facts) or ‘skips’. After creating our labels with the previous layer, some skips will have visible labels and some won’t. The skips that do will need to be kept in the flow graph, because they might be jumped to by other facts. The skips that don’t are useless, and are removed by this layer.

#### identifyImplicitParams()

In BIL, function parameters are quite annoying. Some, such as argc and argv for main(), are explicitly listed below the function header. However, some aren’t listed at all. We call these ‘implicit parameters’, and as far as I’ve seen, every parameter for non-main() functions are this type. In BIL, parameters are passed between functions using registers. Hence, if we identify a register being assigned to a memory address within the stack at the start of a function (before it has been assigned any value from within the function), then we can assume that this register represents a parameter, and we can identify the assigned memory address as the location of this variable within the stack. This layer identifies this pattern, and creates parameter objects from these implicit parameters.

#### resolveInParams()

After all parameters (implicit and explicit) have been created in the previous layer, this layer replaces all references to their memory addresses with their human-readable names. It also removes the initial storing of their associated registers in their associated memory addresses.

#### resolveOutParams()

We refer to the parameters that are passed in as input as ‘in-parameters’, and any returned variable as the ‘out-parameter’. This layer does a similar thing to what the previous layer did with in-parameters, except with out-parameters instead. In other words, we’re replacing all references to the register being used to carry the return value, with the human-readable parameter name.

#### resolveVars()

This layer attempts to remove as many redundant register assignments as possible, by using constant propagation. For example, if, within the same block, we have these two lines:

    X0 := 1
    X0 := X0 + 3

…we fold this into one assignment:

    X0 := 1 + 3

…and then compute the value of the RHS (this functionality is not yet implemented):

    X0 := 4

Now if we encounter the line:

    X1 := X0

…we replace this with:

    X1 := 4

Our ‘dream’ is to remove all references to registers by removing them where they’re not strictly necessary, and replacing them with human-readable variable names where they are.

#### addVarDeclarations()

In Boogie, all local variables need to be initialised at the start of the function. This layer adds initialisation facts at the start of the function, for all local variables referenced within the function.

### Writing to a File

Facts override the toString() method such that they can be printed as syntactically-valid Boogie lines of code. Similarly, Blocks can be printed as syntactically-valid Boogie basic blocks, consisting of their label, followed by the toString() output of each of their Facts. Functions are similar in relation to Blocks, and FlowGraphs are similar in relation to Functions. Hence, converting the flow graph to valid Boogie syntax to be written to a file is as simple as using the FlowGraph.toString() method.

## Tips for Programmers

Some objects returned by the flow graph, such as lists of lines and blocks, may not be member variables. In other words, they may have been freshly made by the called getter, so modifying them will have no effect on the flow graph. However, others indeed may be member variables. This should change soon, but be aware of exactly what objects you're modifying when trying to modify the flow graph.

Recall that expression facts override equals and instruction facts don't. Hence, instructionFact1.equals(instructionFact2) is always false.

Types for children of facts are fairly strict, and although we intend to loosen them soon, bear in mind we might get a lot of cast exceptions before that happens.

We want flow graphs to maintain particular properties or guarantees. Use FlowGraph.enforceConstraints() to ensure these guarantees are met. This is particularly useful after editing a flow graph.

## Tasks for the Summer

todo

 - Value analysis 
 - Points to/alias analysis
 - Reading globals from the symbol table and global offset table
 - Using the call graph
 - Generate the verification conditions

