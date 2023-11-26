We want general specs for standard library functions and with this example 
we made some progress on that front. 

Generalising specifications often requires quantification to summarise loops,
and this requires dealing with the challenges of triggers. 

### Triggers

In short quantifications are not instantiated in Z3 unless they are 'triggered'
by some existing facts pattern-matching against a chosen subexpression of the
quantification. Normally boogie and Z3 try to choose triggers, but can 
struggle if the quantification is not in a "nice" form. Without a good trigger
Z3 responds as if the quantification does not exist, appearing to give "incorrect"  
counterexamples.

In short quantifications which have simple sub-expressions in the left-hand-side 
(i.e. the facts we begin with and try to deduce from) to trigger instantiation on 
work better. 

Quantification instantiation can also be less reliable when you would
want to trigger on expressions hidden behind a function call, 
so it is more reliable to write `mem[i]` than `memory_load8_le(mem, i)`.


For example, for the `memset` function the following **does** work:

```bpl
procedure memset();
  modifies mem, Gamma_mem;
  ensures (forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then Gamma_R1 else old(Gamma_mem[i])));
  // Works
  //ensures (forall i: bv64 :: {mem[i]} (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))));
  // Faster 
  ensures (forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))));
  // Does not verify 
  // ensures (forall i: bv64 :: (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))));
```

Similarly for `memcpy`


```bpl
procedure memcpy();
  modifies mem, Gamma_mem;
  // Works
  //ensures (forall i: bv64 :: {mem[i]} (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));
  // Faster 
  ensures (forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));
  // Does not verify 
  // ensures (forall i: bv64 :: (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));
```

(Thank you to Nick for these examples)

Similarly `strlen()` works in this form,

```bpl
procedure strlen();
  modifies R0, Gamma_R0;
  ensures Gamma_R0 == true;
  ensures (forall i: bv64 :: (bvule64(old(R0), i)) && (bvult64(i, bvadd64(old(R0), R0))) ==> mem[i] != 0bv8);
  ensures (memory_load8_le(mem, bvadd64(old(R0), R0)) == 0bv8);
  ensures (bvule64(old(R0), bvadd64(old(R0), R0)));
```

But **does not** work when we do the address calculation in the memory access rather than the quantification bound. 

```bpl
procedure strlen();
  // does not work
  modifies R0, Gamma_R0;
  ensures Gamma_R0 == true;
  ensures (forall i: bv64 :: (bvule64(0bv64, i)) && (bvult64(i, R0)) ==> mem[bvadd64(old(R0), i)] != 0bv8);
  ensures (memory_load8_le(mem, bvadd64(old(R0), R0)) == 0bv8);
  ensures (bvule64(old(R0), bvadd64(old(R0), R0)));
```

### Dafny inferred triggers

Dafny can also infer triggers, which may be worth comparing. To get Dafny to emit its 
generated boogie code run it with `dafny -print:a.bpl b.dfy`. 

- https://leino.science/papers/krml253.pdf
- https://github.com/dafny-lang/dafny/wiki/FAQ#how-does-dafny-handle-quantifiers-ive-heard-about-triggers-what-are-those
