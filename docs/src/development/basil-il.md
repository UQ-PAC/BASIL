
# Basil Intermediate Language (IL)

Basil IL is the canonical textual representation of the basil IR datastructure.
It can be serialised and loaded back into IR, and aims to be terse and human readable but precise and unambiguous.

Below is an example for a procedure extracted from CNTLM.

[There is a grammar available here](https://uq-pac.github.io/BASIL/docs/basil-il/BasilIR.html).

```
memory shared $mem : (bv64 -> bv8);
memory $stack : (bv64 -> bv8);
var $R0: bv64;
var $R29: bv64;
var $R30: bv64;
var $R31: bv64;
var $R8: bv64;
var $R9: bv64;

prog entry @main_1812;

proc @main_1812 () -> ()
  { .name = "main"; .address = 0x714 }
[
  block %main_entry {.address = 0x714; .originalLabel = "EOZN5uSLQQqIXnLhnZHeWA=="} [
    var Cse0__5_7: bv64 := bvadd($R31:bv64, 0xfffffffffffffff0:bv64) { .label = "1812_0" };
    store le $stack Cse0__5_7:bv64 $R29:bv64 64 { .label = "1812_1" };
    store le $stack bvadd(Cse0__5_7:bv64, 0x8:bv64) $R30:bv64 64 { .label = "1812_2" };
    $R31: bv64 := Cse0__5_7:bv64 { .label = "1812_3" };
    $R29: bv64 := $R31:bv64 { .label = "1816_0" };
    $R9: bv64 := 0x11000:bv64 { .label = "1820_0" };
    $R8: bv64 := 0x1:bv64 { .label = "1824_0" };
    store le $mem bvadd($R9:bv64, 0x34:bv64) extract(32, 0, $R8:bv64) 32 { .label = "1828_0" };
    $R30: bv64 := 0x72c:bv64 { .label = "1832_0" };
    call @get_two_1856 ();
    goto(%main_3);
  ];
  block %main_3 {.address = 0x72c; .originalLabel = "Q55zdQssRl+zLCCVlSDCuw=="} [
    $R8: bv64 := 0x11000:bv64 { .label = "1836_0" };
    store le $mem bvadd($R8:bv64, 0x38:bv64) extract(32, 0, $R0:bv64) 32 { .label = "1840_0" };
    $R0: bv64 := 0x0:bv64 { .label = "1844_0" };
    var load19: bv64 := load le $stack $R31:bv64 64 { .label = "1848_0_0" };
    $R29: bv64 := load19:bv64 { .label = "1848_0_1" };
    var load20: bv64 := load le $stack bvadd($R31:bv64, 0x8:bv64) 64 { .label = "1848_1_0" };
    $R30: bv64 := load20:bv64 { .label = "1848_1_1" };
    $R31: bv64 := bvadd($R31:bv64, 0x10:bv64) { .label = "1848_2" };
    goto(%main_basil_return_1);
  ];
  block %main_basil_return_1 [
    return ();
  ]
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

