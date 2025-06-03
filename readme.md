# BASIL (Boogie Analysis for Secure Information-Flow Logics)

BASIL generates semantically equivalent Boogie source files (`.bpl`) from AArch64/ARM64 
binaries that have been lifted intermediate formats. It takes as input the `.gts` format produced by 
[gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics),  which consists of [ddisasm's](https://github.com/grammatech/ddisasm)
GTIRB  with [ASLp's](https://github.com/UQ-PAC/aslp) instruction semantics annotated as AuxData for each block.

### Information flow logic example

Basil implements a concurrent information-flow logic verifier by encoding this logic in Boogie. 
This logic is described [here](docs/iflogic-encoding.pdf). Verifying examples can be found in 
`src/test/correct` and non-verify examples can be found in `src/test/incorrect`.

```sh
$ ./mill run --load-directory-gtirb src/test/correct/secret_write/gcc -o test.bpl --verify
[193/193] run
[193] [INFO]   Found src/test/correct/secret_write/gcc/secret_write.gts src/test/correct/secret_write/gcc/secret_write.relf src/test/correct/secret_write/secret_write.spec
[193] [INFO]   [!] Loading Program
[193] [INFO]   [!] Removed all PC-related statements
[193] [INFO]   [!] Removing external function calls
[193] [INFO]   [!] Removed unreachable blocks
[193] [INFO]   [!] Stripping unreachable
[193] [INFO]   [!] Removed 17 functions (1 remaining)
[193] [INFO]   [!] Translating to Boogie
[193] [INFO]   Writing output
[193] [INFO]   Running: boogie /useArrayAxioms test.bpl
[193] [INFO]   Boogie result: Verified(4,0)
[193]          
[193] [INFO]   PerformanceTimer Verify [Finish]: 955ms
[193/193] ============================== run --load-directory-gtirb src/test/correct/secret_write/gcc -o test.bpl --verify ============================== 1s
$ tail test.bpl
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    assume {:captureState "1912_0"} true;
    R0, Gamma_R0 := 0bv64, true;
    goto main_1812_basil_return;
  main_1812_basil_return:
    assume {:captureState "main_1812_basil_return"} true;
    return;
}
```

```sh
$ ./mill run --load-directory-gtirb src/test/incorrect/basicassign/gcc --verify
[193/193] run
[193] [INFO]   Found src/test/incorrect/basicassign/gcc/basicassign.gts src/test/incorrect/basicassign/gcc/basicassign.relf src/test/incorrect/basicassign/basicassign.spec
[193] [INFO]   [!] Loading Program
[193] [INFO]   [!] Removed all PC-related statements
[193] [INFO]   [!] Removing external function calls
[193] [INFO]   [!] Removed unreachable blocks
[193] [INFO]   [!] Stripping unreachable
[193] [INFO]   [!] Removed 17 functions (1 remaining)
[193] [INFO]   [!] Translating to Boogie
[193] [INFO]   Writing output
[193] [INFO]   Running: boogie /useArrayAxioms basil-out.bpl
[193] basil-out.bpl(143,5): Error: this assertion could not be proved
[193] Execution trace:
[193]     basil-out.bpl(94,3): main_1812__0__BKOmkvhNSN~7Fh58nX3c2Q
[193] 
[193] Boogie program verifier finished with 2 verified, 1 error
[193] 
[193] [INFO]   Boogie result: AssertionFailed
[193]          Failing assertion: basil-out.bpl:143
[193]              140 |     R0, Gamma_R0 := 69632bv64, true;
[193]              141 |     R0, Gamma_R0 := bvadd64(R0, 24bv64), Gamma_R0;
[193]              142 |     call rely();
[193]           >  143 |     assert (L(mem, R0) ==> Gamma_R1);
[193]              144 |     mem, Gamma_mem := memory_store32_le(mem, R0, R1[32:0]), gamma_store32(Gamma_mem, R0, Gamma_R1);
[193]              145 |     assume {:captureState "1916_0"} true;
[193]              146 |     R0, Gamma_R0 := 0bv64, true;
[193] [INFO]   PerformanceTimer Verify [Finish]: 839ms
[193] Exception in thread "main" java.lang.Exception: Verification failed
[193]   at Main$.main(Main.scala:325)
[193]   at Main.main(Main.scala)
[193/193, 1 failed] ============================== run --load-directory-gtirb src/test/incorrect/basicassign/gcc --verify ============================== 1s
1 tasks failed
```

### Usage

The `./mill` script should be sufficient to bootstrap Scala, provided you have a JVM (>17) installed.
The test suite (and `--verify` flag) additionally require Boogie and Z3.

As input BASIL requires a `.gts` file produced by `gtirb-semantics`,
as well as a file containing the output of readelf (here denoted with `.relf`), both created from the same AArch64/ARM64 binary, 
and outputs a semantically equivalent .bpl Boogie-language source file.

Detailed instructions for lifting examples and running BASIL can be found at [docs/usage](/docs/usage.md).

Basil also accepts BAP `.adt` files in place of the `.gts` file, however this feature is no longer actively maintained.

To build and run the tool use one of the following commands:

Linux / Mac OS:

```
./mill run --load-directory-gtirb ./example.gts [--output output.bpl] [--simplify] [--interpret]
```

Windows: 

```
./mill.bat run --load-directory-gtirb ./example.gts [--output output.bpl] [--simplify] [--interpret]
```

Other flags are listed below:

```
BASIL                                                                                                
  --load-directory-bap <str>      Load relf, adt, and bir from directory (and spec from parent       
                                  directory)                                                         
  --load-directory-gtirb <str>    Load relf and gts from directory (and spec from parent directory)  
  -i --input <str>                BAP .adt file or GTIRB/ASLi .gts file                              
  -r --relf <str>                 Name of the file containing the output of 'readelf -s -r -W'.      
  -s --spec <str>                 BASIL specification file.                                          
  -o --output <str>               Boogie output destination file.                                    
  --boogie-use-lambda-stores      Use lambda representation of store operations.                     
  --boogie-procedure-rg <str>     Switch version of procedure rely/guarantee checks to emit.         
                                  (function|ifblock)                                                 
  -v --verbose                    Show extra debugging logs (the same as -vl log)                    
  --vl <str>                      Show extra debugging logs for a specific logger (log.DSA, log,     
                                  log.DSA.Constraint Gen, log.analysis, log.analysis.steensgaard,    
                                  log.analysis.procedure-summaries, log.debugdumpir,                 
                                  log.analysis.vsa, log.simplify, log.DSA.SadDSA, log.Stack,         
                                  log.analysis-results-dot, log.analysis.mra, log.DSA.SVA).          
  --analyse                       Run static analysis pass.                                          
  --interpret                     Run BASIL IL interpreter.                                          
  --dump-il <str>                 Dump the Intermediate Language to text.                            
  -m --main-procedure-name <str>  Name of the main procedure to begin analysis at.                   
  --procedure-call-depth <int>    Cull procedures beyond this call depth from the main function      
                                  (defaults to Int.MaxValue)                                         
  --trim-early                    Cull procedures BEFORE running analysis                            
  -h --help                       Show this help message.                                            
  --analysis-results <str>        Log analysis results in files at specified path.                   
  --analysis-results-dot <str>    Log analysis results in .dot form at specified path.               
  -t --threads                    Separates threads into multiple .bpl files with given output       
                                  filename as prefix (requires --analyse flag)                       
  --parameter-form                Lift registers to local variables passed by parameter              
  --summarise-procedures          Generates summaries of procedures which are used in                
                                  pre/post-conditions (requires --analyse flag)                      
  --generate-rely-guarantees      Generates rely-guarantee conditions for each procedure that        
                                  contains a return node.                                            
  --simplify                      Partial evaluate / simplify BASIL IR before output (implies        
                                  --parameter-form)                                                  
  --pc <str>                      Program counter mode, supports GTIRB only. (options: none | keep | 
                                  assert) (default: none)                                            
  --validate-simplify             Emit SMT2 check for validation of simplification expression        
                                  rewrites 'rewrites.smt2'                                           
  --verify                        Run boogie on the resulting file                                   
  --memory-regions <str>          Performs static analysis to separate memory into discrete regions  
                                  in Boogie output (requires --analyse flag) (mra|dsa) (dsa is       
                                  recommended over mra)                                              
  --no-irreducible-loops          Disable producing irreducible loops when --analyse is passed (does 
                                  nothing without --analyse)
  --dsa <str>                     Perform Data Structure Analysis if no version is specified perform 
                                  constraint generation (requires --simplify flag)                   
                                  (none|norm|field|set|all)                                          
  --memory-transform              Transform memory access to region accesses                         
  --noif                          Disable information flow security transform in Boogie output       
```

## Development Setup

See [docs/development](docs/development)


## Open Source License

Copyright 2025 The University of Queensland

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at:

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
