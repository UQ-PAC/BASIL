
# Basil Intermediate Language (IL)

Basil IL is the canonical textual representation of the basil IR datastructure.
It can be serialised and laoded back into IR, and aims to be terse and human readable but precise and unambiguous.

Below is an example for a procedure extracted from CNTLM.

[There is a grammar available here](https://uq-pac.github.io/BASIL/docs/basil-il/BasilIR.html).

```
memory shared $mem : (bv64 -> bv8);

prog entry @main_1860;

proc @main_1860
  (R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, R8_in:bv64, _PC_in:bv64)
    -> (R0_out:bv64, R29_out:bv64, R30_out:bv64, R8_out:bv64, _PC_out:bv64)
  { .name = "main"; .address = 0x744 }
  require eq(_PC_in:bv64, 0x744:bv64);
  ensures eq(_PC_out:bv64, R30_in:bv64);
[
  block %main_entry {.address = 0x744; .originalLabel = "rboiQVDCQyC6FKdIfNmCPw=="} [
    assert eq(_PC_in:bv64, 0x744:bv64) { .label = "pc-tracking"; .comment = "pc-tracking";  };
    store le $mem bvadd(R31_in:bv64, 0xfffffffffffffff0:bv64) R29_in:bv64 64;
    store le $mem bvadd(R31_in:bv64, 0xfffffffffffffff8:bv64) R30_in:bv64 64;
    store le $mem bvadd(R31_in:bv64, 0xffffffffffffffec:bv64) 0x0:bv32 32;
    store le $mem bvadd(R31_in:bv64, 0xffffffffffffffe8:bv64) extract(32, 0, R0_in:bv64) 32;
    var load15_1: bv32 := load le $mem bvadd(R31_in:bv64, 0xffffffffffffffe8:bv64) 32;
    store le $mem bvadd(R31_in:bv64, 0xffffffffffffffdc:bv64) load15_1:bv32 32;
    goto(%phi_1, %phi_2);
  ];
];
```


## Identifiers and Sigils

Sigils are identifier prefix characters used to disambiguate different types of identifier. Basil IL has 4 types of identifier:

- Procedure identifiers with the sigil `@`
- Blocks with the sigil `%`
- Local variables with the _optional_ sigil `#`, no other identifiers may begin with a `#`.
- Global variables/registers (and mem, functions, axioms) with the sigil `$`
- Specification/attribute identifiers with the sigil `.`

## Parsing and Printing Basil IL

- Basil IL constructs have method `dumpString` defined which prints the full textual representation.
- The parser can be tested by running `./mill bnfc ./basilfile-output.il`

## Additional Tooling

A language server that provides basic syntax checking and definition jumping can be found [here](https://github.com/ailrst/basls).

