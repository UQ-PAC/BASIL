
function {:bvbuiltin "bvuge"} bvuge64(bv64, bv64) returns (bool);
function {:bvbuiltin "bvugt"} bvugt64(bv64, bv64) returns (bool);


var malloc_count: int;
var malloc_base: [int]bv64;
var malloc_end: [int]bv64;
var malloc_id: [bv64]int;

var Gamma_malloc_count: bool;
var Gamma_malloc_base: bool;
var Gamma_malloc_end: bool;
var Gamma_malloc_id: bool;


// because named wrong in generated file
function {:inline} in_bounds64(base: bv64, len: bv64, i: bv64) returns (bool) {
  (if bvule64(base, bvadd64(base, len)) then (bvule64(base, i) && bvult64(i, bvadd64(base, len))) else (bvule64(base, i) || bvult64(i, bvadd64(base, len))))
}
