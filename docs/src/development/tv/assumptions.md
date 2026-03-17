# Lifter Assumptions

A transform pass may want drive a code-transform with a reasonable assumption
about the behaviour, that it cannot technically prove with analysis.

One such example is the `AssumeCallPreserved` pass which modifies
procedure parameter lists on the assumption that the program
respects the arm64 calling convention.

To allow this, we introduce an assertion to the program, encoding the
assumption that was made by the transform. When the program
is eventually verified in Boogie, we discharge this assumption,
proving that our assumption was sound.

The translation validation pipeline proves that as long as 
the assumption holds, the programs have the same behaviour.
Because the introduction of an assert statement changes the
program behaviour, we need to add the specification that
traces on which the assertion fails can be ignored.

Procedure procedure `_start` is an interesting example.

We can use the script in `scripts/soundnesslitmuscvc5wrapper.sh` to get the number of
program statements contributing to the unsat core of the program:


```

tvsmt/simplifyCFG-_start_4213248.smt2
66% contributed  	171/259 asserts

tvsmt/Parameters-_start_4213248.smt2
66% contributed  	145/219 asserts

tvsmt/AssumeCallPreserved-_start_4213248.smt2
68% contributed  	177/260 asserts

tvsmt/DSA-_start_4213248.smt2
53% contributed  	169/316 asserts

tvsmt/CopyProp-_start_4213248.smt2
4% contributed  	11/257 asserts

tvsmt/GuardCleanup-_start_4213248.smt2
7% contributed  	10/138 asserts

```

The procedure `_start` contains an unresolved indirect tail call, 
so the pass AssumeCallPreserved injects assertions that don't hold (but TV passes as its not TV's responsibility to prove them), 
but then subsequent analyses that leverage this assumption are not sound and we get tv passing trivially due to 
`assert eq(0x404a34:bv64, 0x404b64:bv64) { .comment = "R30 = R30_in" }; ~> assert false`. This is the expected behavior, a 
program with an assert false is obviously a program that doesn't verify.

This unsat core is showing roughly that the translation validation is passing vacuously because
the copyprop transform has derived false from the assertion we introduced.

```
cvc5 --dump-unsat-cores tvsmt/CopyProp-_start_4213248.smt2
unsat
(
source5
source47
source54
source56
source51
source57
source53
source58
source69
source70
source48
)
```

<details>
    <summary>full _start IL after copyprop pass</summary>

```

  (R0_in:bv64, R10_in:bv64, R11_in:bv64, R12_in:bv64, R13_in:bv64, R14_in:bv64, R15_in:bv64, R16_in:bv64, R17_in:bv64, R18_in:bv64, R1_in:bv64, R29_in:bv64, R2_in:bv64, R30_in:bv64, R31_in:bv64, R3_in:bv64, R4_in:bv64, R5_in:bv64, R6_in:bv64, R7_in:bv64, R8_in:bv64, R9_in:bv64, _PC_in:bv64)
    -> (R0_out:bv64, R1_out:bv64, R2_out:bv64, R3_out:bv64, R4_out:bv64, R5_out:bv64, R6_out:bv64, R7_out:bv64, _PC_out:bv64)
  { .name = "_start"; .address = 0x404a00 }
[
  block %_start_entry {.address = 0x404a00; .originalLabel = "ufjL9zmpTde18uF80OwPVQ=="} [
    var R5_2: bv64 := bvor(0x0:bv64, bvshl(R0_in:bv64, 0x0:bv64));
    var var1_4213376_bv64_1: bv64 := load le $mem R31_in:bv64 64;
    var var2_4202816_bv64_1: bv64 := load le $mem bvadd(0x430000:bv64, 0x40:bv64) 64;
    assert eq(0x404b64:bv64, 0x404b64:bv64) { .comment = "R30 = R30_in" };
    var (R0_4:bv64, R10_2:bv64, R11_2:bv64, R12_2:bv64, R13_2:bv64, R14_2:bv64, R15_2:bv64, R16_4:bv64, R17_3:bv64, R18_2:bv64, R1_3:bv64, R29_3:bv64, R2_3:bv64, R3_3:bv64, R4_3:bv64, R5_3:bv64, R6_3:bv64, R7_2:bv64, R8_2:bv64, R9_2:bv64)
        := call @__libc_start_main (0x404a34:bv64, R10_in:bv64, R11_in:bv64, R12_in:bv64, R13_in:bv64, R14_in:bv64, R15_in:bv64, 0x430040:bv64, var2_4202816_bv64_1:bv64, R18_in:bv64, var1_4213376_bv64_1:bv64, 0x0:bv64, bvadd(R31_in:bv64, 0x8:bv64), 0x404b64:bv64, 0x0:bv64, 0x0:bv64, R5_2:bv64, R31_in:bv64, R7_in:bv64, R8_in:bv64, R9_in:bv64);
    goto(%phi_5);
  ];
  block %_start_10 {.address = 0x404a30; .originalLabel = "eH7LoljnQS6XhgPVv4Qipg=="} [
    assert eq(0x404a34:bv64, 0x404b64:bv64) { .comment = "R30 = R30_in" };
    var var2_4203488_bv64_1: bv64 := load le $mem bvadd(0x430000:bv64, 0x190:bv64) 64;
    assert eq(0x404a34:bv64, 0x404a34:bv64) { .comment = "R30 = R30_in" };
    var (R0_6:bv64, R10_4:bv64, R11_4:bv64, R12_4:bv64, R13_4:bv64, R14_4:bv64, R15_4:bv64, R16_7:bv64, R17_5:bv64, R18_4:bv64, R1_5:bv64, R29_5:bv64, R2_5:bv64, R3_5:bv64, R4_5:bv64, R5_5:bv64, R6_5:bv64, R7_4:bv64, R8_4:bv64, R9_4:bv64)
        := call @abort (R0_4:bv64, R10_2:bv64, R11_2:bv64, R12_2:bv64, R13_2:bv64, R14_2:bv64, R15_2:bv64, 0x430190:bv64, var2_4203488_bv64_1:bv64, R18_2:bv64, R1_3:bv64, R29_3:bv64, R2_3:bv64, 0x404a34:bv64, R3_3:bv64, R4_3:bv64, R5_3:bv64, R6_3:bv64, R7_2:bv64, R8_2:bv64, R9_2:bv64);
    goto(%phi_6);
  ];
  block %phi_5 {.originalLabel = "eH7LoljnQS6XhgPVv4Qipg==, ufjL9zmpTde18uF80OwPVQ=="} [
    goto(%_start_10);
  ];
  block %phi_6 {.originalLabel = "eH7LoljnQS6XhgPVv4Qipg=="} [
    goto(%_start_return);
  ];
  block %_start_return [
    return (R0_6:bv64, R1_5:bv64, R2_5:bv64, R3_5:bv64, R4_5:bv64, R5_5:bv64, R6_5:bv64, R7_4:bv64, _PC_in:bv64);
  ]
];
```
</details>
