#!/usr/bin/env python3
# vim: ts=2 sts=2 et sw=2

"""
BAP .adt / .bir file normaliser:

usage:
bap-normalise.py ADT-FILE BIR-FILE

both arguments are required in that order!
files will be modified in-place.

+00000540: main_argv :: in out u64 = R1
+00000541: main_result :: out u32 = low:32[R0]
 
 00000199:
 0000019c: R1 := 0x41F000
@@ -187,7 +187,7 @@
 000001c8: call R30 with noreturn
 
 00000515: sub register_tm_clones(register_tm_clones_result)
-00000529: register_tm_clones_result :: out u32 = low:32[R0]
+00000542: register_tm_clones_result :: out u32 = low:32[R0]
 
 0000028e:
 00000291: R0 := 0x420000
@@ -199,13 +199,13 @@
 000002b6: R1 := R2 + (R1 ~>> 3)
 000002bc: R1 := extend:64[63:1[R1]]
 000002c2: when R1 = 0 goto %000002c0
-00000516: goto %00000337
+0000052f: goto %00000337
 
 00000337:
 0000033a: R2 := 0x41F000
 00000341: R2 := mem[R2 + 0xFF8, el]:u64
 00000346: when R2 = 0 goto %000002c0
-00000517: goto %0000034a
+00000530: goto %0000034a
 
"""

import sys
import re

adt_file = sys.argv[1]
bir_file = sys.argv[2]
assert len(sys.argv) == 3

string_re = re.compile(rb'''"((?:[^"\\]|\\.)*)"''')
hexstring_re = re.compile(rb'''"%([\da-fA-F]{8})"''')
tid_re = re.compile(rb'''Tid\(([_\d]+),''')
bir_re = re.compile(rb'''(?:^([\da-fA-F]{8}):)|(?: %([\da-fA-F]{8}))''', re.MULTILINE)

with open(adt_file, 'rb') as f:
  adt = f.read()

tids: dict[int, int] = {}  # map of old tid to their first position in adt
for match in re.finditer(tid_re, adt):
  tid = int(match[1].replace(b'_', b''))
  if tid not in tids:
    tids[tid] = match.start()

assert tids, f'adt file {adt_file} has no Tid() values??'

keys = list(tids.keys())
keys.sort(key=tids.__getitem__)

new_tids = {tid: 4*i for i, tid in enumerate(keys)}

# .adt file

def sub_adt(m: re.Match[bytes]) -> bytes:
  tid = int(m[1].replace(b'_', b''))
  new = new_tids[tid]
  return f'Tid({new:_},'.encode('ascii')
def sub_adt_strings(m: re.Match[bytes]) -> bytes:
  tid = int(m[1], 16)
  new = new_tids[tid]
  return f'"%{new:08x}"'.encode('ascii')

new_adt = re.sub(tid_re, sub_adt, adt)
new_adt = re.sub(hexstring_re, sub_adt_strings, new_adt)

# .bir file

# print(new_tids)
bir_seen = set()
def sub_bir(m: re.Match[bytes]) -> bytes:
  old = m[1] or m[2]
  tid = int(old, 16)
  bir_seen.add(tid)
  assert tid in new_tids, f"{m}"
  new = new_tids[tid]
  return m[0].replace(old, f'{new:08x}'.encode('ascii'))

with open(bir_file, 'rb') as f:
  bir = f.read()

new_bir = re.sub(bir_re, sub_bir, bir)
adt_seen = set(new_tids)
assert bir_seen == adt_seen, f'not equal!\nbir - adt =\n{bir_seen - adt_seen}\nadt - bir =\n{adt_seen - bir_seen}'

assert new_adt != adt
assert new_bir != bir

# writeback only if both are successful

with open(bir_file, 'wb') as f:
  f.write(new_bir)

with open(adt_file, 'wb') as f:
  f.write(new_adt)
