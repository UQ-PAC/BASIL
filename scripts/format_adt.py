#!/usr/bin/env python3
# vim: noai:ts=2:sw=2:expandtab

"""
format_adt.py implements pretty-printing of BAP .adt files
by translating the ADT into Python syntax, then parsing and
formatting the python.

Although this eval()s, it is made safe by using ast.literal_eval
which only supports parsing of a literal Python expression.
"""

import re
import ast
import sys
import pprint
import typing
import argparse

# cd examples
# grep -E '[A-Z][a-ZA-Z]+\\(' **/*.adt --no-filename --only-matching | sort | uniq | tr -d '(' | xargs -n1 printf "'%s', " | fold -w80 -s
heads = [
  'AND', 'Annotation', 'Arg', 'Args', 'ARSHIFT', 'Attr', 'Attrs', 'Blk', 'Blks', 
  'Both', 'Call', 'Concat', 'Def', 'Defs', 'Direct', 'EQ', 'Extract', 'Goto', 
  'Imm', 'In', 'Indirect', 'Int', 'Jmps', 'LittleEndian', 'Load', 'LOW', 
  'LSHIFT', 'Mem', 'Memmap', 'MINUS', 'NEQ', 'NOT', 'OR', 'Out', 'Phis', 'PLUS', 
  'Program', 'Project', 'Region', 'RSHIFT', 'SDIVIDE', 'Section', 'Sections', 
  'SIGNED', 'SLT', 'Store', 'Sub', 'Subs', 'Tid', 'TIMES', 'UNSIGNED', 'Var', 
  'XOR',
]
heads_joined = '|'.join(heads)

def preprocess(data: str) -> str:
  """
  Preprocesses BAP ADT intrinsics (like Program, Subs, ...) into tuple syntax.
  For example, Program(1, 2, 3) becomes ("Program", 1, 2, 3).
  """
  heads_re = re.compile(f'({heads_joined})[(]')

  data = heads_re.sub(lambda x: '(' + repr(x[1]) + ', ', data)
  return data

class DoubleQuoteStr(str):
  def __repr__(self): 
    # TODO: maybe make more robust?
    r = super().__repr__()
    r = r[1:-1]
    r = r.replace(r"\'", r"'")
    r = r.replace(r'"', r'\"')
    return '"' + r + '"'

class UnderscoreInt(int):
  def __repr__(self):
    return f'{self:_}'

Exp = tuple | list | str | int 
def clean(data: Exp) -> Exp:
  """
  Intermediate step before formatting to tweak pprint's formatting.
  This ensures we match BAP as close as possible with double-quoted strings
  and underscores in Tid.
  """
  if isinstance(data, tuple) and data[0] == 'Tid' and not isinstance(data[1], UnderscoreInt):
    return clean((data[0], UnderscoreInt(data[1]), ) + data[2:])
  if isinstance(data, str):
    return DoubleQuoteStr(data)
  if isinstance(data, (list, tuple)):
    return data.__class__(map(clean, data))
  return data

def postprocess(data: str) -> str:
  """
  Postprocesses the formatted Python expression to restore the BAP-style intrinsics.
  """
  heads_re2 = re.compile(f'[(]"({heads_joined})",(\\s)')

  data = heads_re2.sub(lambda x: x[1] + '(' + ('\n' if x[2] == '\n' else ''), data)
  data = data.replace(',)', ')')
  return data


def main(args):
  infile = args.input
  outfile = args.output
  width = args.width
  update = args.update

  data = infile.read()

  out = data
  out = preprocess(out)
  out = ast.literal_eval(out)
  out = clean(out)
  out = pprint.pformat(out, indent=width, underscore_numbers=False)
  out = postprocess(out)

  if update:
    infile.close()
    with open(infile.name, 'w') as outfile:
      outfile.write(out)
      outfile.write('\n')
  else:
    outfile.write(out)
    outfile.write('\n')
    outfile.flush()

if __name__ == '__main__':
  argp = argparse.ArgumentParser(description="pretty formats BAP ADT files.")
  argp.add_argument('input', nargs='?', type=argparse.FileType('r'), default=sys.stdin,
                    help="input .adt file (default: stdin)")
  excl = argp.add_mutually_exclusive_group()
  excl.add_argument('output', nargs='?', type=argparse.FileType('w'), default=sys.stdout,
                    help="output file name (default: stdout)")

  argp.add_argument('--width', '-w', default=1, type=int, 
                    help="indent size in spaces (default: 1)")

  excl.add_argument('--update', '-i', action='store_true',
                    help="write output back to the input file (default: false)")

  args = argp.parse_args()
  
  if args.input is sys.stdin and args.update:
    argp.error('argument --update/-i: not allowed with stdin input')

  main(args)

