#!/usr/bin/env python3
# vim: ts=2 sts=2 et sw=2

import os
import sys
from pathlib import Path

"""
For /very/ large files, the Scala produced by ToScalaWithSplitting may still be too large
since the combined code may exceed the class size limit.

This script will take the output from ToScalaWithSplitting and separate each function definition
into its own .scala file.

At the moment, the split .scala files are all in the package `ir.dsl.generated`.

usage:

    scripts/split_toscala_output.py cntlm-output.scala src/main/scala/ir/dsl/generated

"""

outdir = Path(sys.argv[2])
os.makedirs(outdir, exist_ok=True)

def new_file(p):
  out = open(p, 'w')
  out.writelines([
    "package ir.dsl.generated\n",
    "import ir.*\n",
    "import ir.dsl.*\n",
  ])
  return out


with open(sys.argv[1], 'r') as f:
  assert '{' == f.readline().strip()
  i = 0
  out = new_file(outdir / f'f{i:04}.scala')
  while (l := f.readline()) and l != '':
    if '' == l.strip():
      out.close()
      i += 1
      out = new_file(outdir / f'f{i:04}.scala')
    elif l.startswith('  ') and not l[3].isspace() and not l[2:].startswith('def '):
      print('found end')
      break
    else:
      out.writelines([l.replace('  ', '', 1)])

