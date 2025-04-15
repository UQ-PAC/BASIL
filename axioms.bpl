

function source__trace_load_8(Tr, bv64) returns (Tr);
function source__load_8(Tr, bv64) returns (bv8);
function source__store_8(Tr, bv64, bv8) returns (Tr);
function target__load_8(Tr, bv64) returns (bv8);
function target__trace_load_8(Tr, bv64) returns (Tr);
function target__store_8(Tr, bv64, bv8) returns (Tr);

function source__trace_load_32(Tr, bv64) returns (Tr);
function source__load_32(Tr, bv64) returns (bv32);
function source__store_32(Tr, bv64, bv32) returns (Tr);
function target__load_32(Tr, bv64) returns (bv32);
function target__trace_load_32(Tr, bv64) returns (Tr);
function target__store_32(Tr, bv64, bv32) returns (Tr);

function source__trace_load_64(Tr, bv64) returns (Tr);
function source__load_64(Tr, bv64) returns (bv64);
function source__store_64(Tr, bv64, bv64) returns (Tr);
function target__load_64(Tr, bv64) returns (bv64);
function target__trace_load_64(Tr, bv64) returns (Tr);
function target__store_64(Tr, bv64, bv64) returns (Tr);


function source__trace_load_128(Tr, bv64) returns (Tr);
function source__load_128(Tr, bv64) returns (bv128);
function source__store_128(Tr, bv64, bv128) returns (Tr);
function target__load_128(Tr, bv64) returns (bv128);
function target__trace_load_128(Tr, bv64) returns (Tr);
function target__store_128(Tr, bv64, bv128) returns (Tr);

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64, source__value: bv8, target__value: bv8 :: 
        {source__store_8(source__tr,source__addr,source__value), target__store_8(target__tr,target__addr,target__value)}
        (source__tr == target__tr && source__addr == target__addr && source__value == target__value) ==> 
        (source__store_8(source__tr,source__addr,source__value) == target__store_8(target__tr,target__addr,target__value)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__load_8(source__tr,source__addr), target__load_8(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__load_8(source__tr,source__addr) == target__load_8(target__tr,target__addr)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__trace_load_8(source__tr,source__addr), target__trace_load_8(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__trace_load_8(source__tr,source__addr) == target__trace_load_8(target__tr,target__addr)));


axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64, source__value: bv32, target__value: bv32 :: 
        {source__store_32(source__tr,source__addr,source__value), target__store_32(target__tr,target__addr,target__value)}
        (source__tr == target__tr && source__addr == target__addr && source__value == target__value) ==> 
        (source__store_32(source__tr,source__addr,source__value) == target__store_32(target__tr,target__addr,target__value)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__load_32(source__tr,source__addr), target__load_32(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__load_32(source__tr,source__addr) == target__load_32(target__tr,target__addr)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__trace_load_32(source__tr,source__addr), target__trace_load_32(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__trace_load_32(source__tr,source__addr) == target__trace_load_32(target__tr,target__addr)));


axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64, source__value: bv64, target__value: bv64 :: 
        {source__store_64(source__tr,source__addr,source__value), target__store_64(target__tr,target__addr,target__value)}
        (source__tr == target__tr && source__addr == target__addr && source__value == target__value) ==> 
        (source__store_64(source__tr,source__addr,source__value) == target__store_64(target__tr,target__addr,target__value)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__load_64(source__tr,source__addr), target__load_64(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__load_64(source__tr,source__addr) == target__load_64(target__tr,target__addr)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__trace_load_64(source__tr,source__addr), target__trace_load_64(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__trace_load_64(source__tr,source__addr) == target__trace_load_64(target__tr,target__addr)));




axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64, source__value: bv128, target__value: bv128 :: 
        {source__store_128(source__tr,source__addr,source__value), target__store_128(target__tr,target__addr,target__value)}
        (source__tr == target__tr && source__addr == target__addr && source__value == target__value) ==> 
        (source__store_128(source__tr,source__addr,source__value) == target__store_128(target__tr,target__addr,target__value)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__load_128(source__tr,source__addr), target__load_128(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__load_128(source__tr,source__addr) == target__load_128(target__tr,target__addr)));

axiom (forall source__tr: Tr, target__tr: Tr, source__addr: bv64, target__addr: bv64 :: 
        {source__trace_load_128(source__tr,source__addr), target__trace_load_128(target__tr,target__addr)}
        (source__tr == target__tr && source__addr == target__addr) ==> (source__trace_load_128(source__tr,source__addr) == target__trace_load_128(target__tr,target__addr)));


