
# Basil Intermediate Language (IL)

Basil IL is the canonical textual representation of the basil IR datastructure.
It can be serialised and laoded back into IR, and aims to be terse and human readable but precise and unambiguous.

Below is an example for a procedure extracted from CNTLM.

There is a grammar available here: https://uq-pac.github.io/BASIL/docs/basil-il/BasilIR.html

```
memory $mem : (bv64 -> bv8);
memory $stack : (bv64 -> bv8);

prog { .entry = "main_1812" };

proc @main_1812
  (R0_in:bv64, R1_in:bv64, R30_in:bv64, R31_in:bv64, _PC_in:bv64)
    -> (R0_out:bv64, _PC_out:bv64)
  { .name = "main"; .address = 0x714 }
  require eq(_PC_in:bv64, 0x714:bv64);
  ensures eq(_PC_out:bv64, R30_in:bv64);
[
  block #main_entry {.address = 0x714} [
    store le $stack bvadd(R31_in:bv64, 0xfffffffffffffffc:bv64) 0x0:bv32 32;
    var load18_1:bv32 := load le $mem 0x11018:bv64 32;
    goto(#phi_3, #phi_4);
  ];
  block #main_3 {.address = 0x740} [
    var load20_1:bv32 := load le $stack bvadd(R31_in:bv64, 0xfffffffffffffffc:bv64) 32;
    goto(#main_basil_return_1);
  ];
  block #phi_3 [
    guard boolnot(eq(load18_1:bv32, 0x0:bv32));
    goto(#main_3);
  ];
  block #phi_4 [
    guard eq(load18_1:bv32, 0x0:bv32);
    var load19_1:bv32 := load le $mem 0x11014:bv64 32;
    store le $stack bvadd(R31_in:bv64, 0xfffffffffffffffc:bv64) load19_1:bv32 32;
    goto(#main_3);
  ];
  block #main_basil_return_1 [
    return (zero_extend(32, load20_1:bv32), _PC_in:bv64);
  ]
];
```


## Identifiers and Sigils

Sigils are identifier prefix characters used to disambiguate different types of identifier. Basil IL has 4 types of identifier:

- Procedure identifiers with the sigil `@`
- Blocks with the sigil `#`
- Local variables with the _optional_ sigil `%`, no other identifiers may begin with a `#`.
- Global variables/registers (and mem, functions, axioms) with the sigil `$`
- Specification/attribute identifiers with the sigil `.`

## Parsing and Printing Basil IL

- Basil IL constructs have method `dumpString` defined which prints the full textual representation.
- The parser can be tested by running `./mill bnfc ./basilfile-output.il`

## Additional Tooling

A language server that provides basic syntax checking and definition jumping can be found [here](https://github.com/ailrst/basls).

