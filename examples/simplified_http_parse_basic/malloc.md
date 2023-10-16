
This has been tested with minimal examples

Allocator gives a new pointer to a region disjoint from all other regions on each call.

Todo: Representation compatible with reusing allocated regions that have been freed.

```bpl
var malloc_count: int;
var malloc_base: [int]bv64;
var malloc_end: [int]bv64;

procedure malloc();
modifies mem, Gamma_mem, malloc_count, malloc_base, malloc_end, R0, Gamma_R0;
requires Gamma_R0 == true;
ensures Gamma_R0 == true;
ensures malloc_count == old(malloc_count) + 1;
ensures bvuge64(malloc_end[malloc_count], malloc_base[malloc_count]);
ensures R0 == malloc_base[malloc_count];
ensures malloc_end[malloc_count] == bvadd64(R0, old(R0));
ensures (forall i: int :: i != malloc_count ==> bvugt64(malloc_base[malloc_count], malloc_end[i]) || bvult64(malloc_end[malloc_count], malloc_base[i]));
ensures (forall i: int :: i != malloc_count ==> malloc_base[i] == old(malloc_base[i]) && malloc_end[i] == old(malloc_end[i]));
ensures bvuge64(R0, 100000000bv64); // above the got
```


