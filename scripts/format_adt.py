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
import time
import pprint
import logging
import argparse
import contextlib

# grep -E "'([A-Z][a-zA-Z]+)'" src/main/antlr4/BAP_ADT.g4 --only-matching --no-filename | sort | uniq | xargs printf "'%s', " | fold -w80 -s
heads = [
  'AND', 'Annotation', 'Arg', 'Args', 'ARSHIFT', 'Attr', 'Attrs', 'BigEndian', 
  'Blk', 'Blks', 'Both', 'Call', 'Concat', 'Def', 'Defs', 'Direct', 'DIVIDE', 
  'EQ', 'Extract', 'Goto', 'HIGH', 'Imm', 'In', 'Indirect', 'Int', 'Jmp', 'Jmps', 
  'LE', 'LittleEndian', 'Load', 'LOW', 'LSHIFT', 'LT', 'Mem', 'Memmap', 'MINUS', 
  'MOD', 'NEG', 'NEQ', 'NOT', 'OR', 'Out', 'Phi', 'Phis', 'PLUS', 'Program', 
  'Project', 'Region', 'RSHIFT', 'SDIVIDE', 'Section', 'Sections', 'SIGNED', 
  'SLE', 'SLT', 'SMOD', 'Store', 'Sub', 'Subs', 'Tid', 'TIMES', 'UNSIGNED', 
  'Var', 'XOR',
]
heads_joined = '|'.join(heads)
heads_joined_b = heads_joined.encode('ascii')

log = logging.getLogger()


def preprocess(data: bytes) -> bytes:
  """
  Preprocesses BAP ADT intrinsics (like Program, Subs, ...) into tuple syntax.
  For example, Program(1, 2, 3) becomes ("Program", 1, 2, 3).
  """
  heads_re = re.compile(b'(' + heads_joined_b + b')[(]')

  data = heads_re.sub(lambda x: b'("' + x[1] + b'", ', data)
  return data

class DoubleQuoteStr(str):
  def __repr__(self): 
    # TODO: maybe make more robust?
    r = super().__repr__()
    r = r[1:-1]
    r = r.replace(r"\'", r"'")
    r = r.replace(r'"', r'\"')
    return '"' + r + '"'

class ByteStr(str):
  def __repr__(self):
    bytes = self.encode('utf-8')
    return '"' + ''.join(f'\\x{b:02x}' for b in bytes) + '"'

class UnderscoreInt(int):
  def __repr__(self):
    return f'{self:_}'

Exp = tuple | list | str | bytes | int 
def clean(data: Exp) -> Exp:
  """
  Intermediate step before formatting to tweak pprint's formatting.
  This ensures we match BAP as close as possible with double-quoted strings
  and underscores in Tid.
  """
  if isinstance(data, tuple):
    if data[0] == 'Tid' and not isinstance(data[1], UnderscoreInt):
      return clean((data[0], UnderscoreInt(data[1]), ) + data[2:])
    if data[0] == 'Section' and not isinstance(data[3], ByteStr):
      return clean(data[:3] + (ByteStr(data[3]), ) + data[4:])
  if isinstance(data, str) and type(data) is str:
    return DoubleQuoteStr(data)
  if isinstance(data, (list, tuple)):
    return type(data)(map(clean, data))
  return data

def postprocess(data: bytes) -> bytes:
  """
  Postprocesses the formatted Python expression to restore the BAP-style intrinsics.
  """
  heads_re2 = re.compile(rb'\("(' + heads_joined_b + rb')",(\s|\))')

  def replacement(x: re.Match) -> bytes:
    head = x[1]
    endc = x[2]
    if endc not in b')\n':
      endc = b''
    return head + b'(' + endc

  data = heads_re2.sub(replacement, data)
  return data


@contextlib.contextmanager
def measure_time(context: str):
  log.info(f'starting {context}')
  start = time.perf_counter()
  yield lambda: time.perf_counter() - start
  log.debug(f'... done in {time.perf_counter() - start:.3f} seconds')


def main(args):
  infile = args.input
  outfile = args.output
  width = args.width
  update = args.update

  with measure_time('read'):
    data = infile.read()
    log.debug(f'    read {len(data):,} characters')

  out = data
  
  with measure_time('preprocess'):
    out = preprocess(out)
  
  with measure_time('parse'):
    out = compile(out, infile.name, 'eval', ast.PyCF_ONLY_AST)

  with measure_time('eval'):
    out = ast.literal_eval(out)

  with measure_time('preprint'):
    out = clean(out)
  with measure_time('pprint'):
    out = pprint.pformat(out, indent=width, underscore_numbers=False)
    out = out.encode('ascii')

  with measure_time('postprocess'):
    out = postprocess(out)

  with measure_time('output'):
    if update:
      infile.close()
      with open(infile.name, 'wb') as outfile:
        outfile.write(out)
        outfile.write(b'\n')
    else:
      outfile.write(out)
      outfile.write(b'\n')
      outfile.flush()

if __name__ == '__main__':
  logging.basicConfig(format='[%(asctime)s:%(name)s@%(filename)s:%(levelname)-7s]\t%(message)s')

  argp = argparse.ArgumentParser(description="pretty formats BAP ADT files.")
  argp.add_argument('input', nargs='?', type=argparse.FileType('rb'), default=sys.stdin.buffer,
                    help="input .adt file (default: stdin)")
  excl = argp.add_mutually_exclusive_group()
  excl.add_argument('output', nargs='?', type=argparse.FileType('wb'), default=sys.stdout.buffer,
                    help="output file name (default: stdout)")

  argp.add_argument('--width', '-w', default=1, type=int, 
                    help="indent size in spaces (default: 1)")

  excl.add_argument('--update', '-i', action='store_true',
                    help="write output back to the input file (default: false)")

  argp.add_argument('--verbose', '-v', action='count', default=0,
                    help="print logging output to stderr (default: no, repeatable)")

  args = argp.parse_args()
  
  if args.input is sys.stdin and args.update:
    argp.error('argument --update/-i: not allowed with stdin input')

  if args.verbose == 0:
    level = logging.WARN
  elif args.verbose == 1:
    level = logging.INFO
  else:
    level = logging.DEBUG
  log.setLevel(level)

  log.debug(str(args))

  with measure_time('format_adt.main'):
    main(args)

