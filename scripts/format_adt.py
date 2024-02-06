#!/usr/bin/env python3
# vim: ts=2:sw=2:expandtab:autoindent

"""
format_adt.py implements pretty-printing of BAP .adt files
by translating the ADT into Python syntax, then parsing and
formatting the python.

Although this eval()s, it is made safe by using ast.literal_eval
which only supports parsing of a literal Python expression.
"""

import io
import re
import ast
import sys
import time
import pprint
import logging
import argparse
import dataclasses
import contextlib

log = logging.getLogger()

notspace_re = re.compile(rb'\S')
head_re = re.compile(rb'[^\s(,;)]+')
num_re = re.compile(rb'[_0-9xa-fA-F]+')
string_re = re.compile(rb'"(?:[^"\\]|\\.)*"')

multiline_keywords = set(
  [b'Project', b'Def', b'Goto', b'Call', b'Sub', b'Blk', b'Arg'] +
  [b'Stmt_Assign', b'Expr_Slices', b'Stmt_If', b'Expr_TApply']
)

@dataclasses.dataclass
class Context:
  begin: int
  closer: bytes
  multiline: bool
  head: bytes | None = None
  index: int = 0  # count of children in this bracketed list

flip = {
  b'(': b')',
  b')': b'(',
  b'[': b']',
  b']': b'[',
}

def pretty(outfile, data: bytes, spaces: int):
  # stack of expression beginning parentheses and their start position
  stack: list[Context] = []
  i = 0
  depth = 0
  head = b''

  indent = b' ' * spaces

  i0 = i - 1
  length = len(data)
  while i < length:
    assert i0 != i
    i0 = i

    c = bytes((data[i],))
    if c.isspace(): 
      while i < length and data[i] in b' \t\r\n':
        i += 1
    elif c.isdigit():
      m = num_re.match(data, i)
      assert m
      outfile.write(m[0])
      i = m.end(0)
    elif c in b',;':
      # XXX: (else stmt1 \n stmt2 ...) is not handled correctly due to significant whitespace
      outfile.write(c)
      i += 1
      if stack[-1].multiline:
        outfile.write(b'\n')
        outfile.write(indent * depth)
      else:
        outfile.write(b' ')
      stack[-1].index += 1
    elif c.isalpha() or c == b'_':
      m = head_re.match(data, i)
      assert m
      i = m.end(0)
      head = m[0]
      outfile.write(head)
      if head == b'else':
        stack[-1].multiline = True
        depth += 1

        outfile.write(b'\n')
        outfile.write(indent * depth)
    elif c in b'([': 
      outfile.write(c)
      i += 1
      islist = c == b'[' and ']' != chr(data[i])
      multiline = islist or head in multiline_keywords
      if stack and stack[-1].head == b'Expr_TApply' and stack[-1].index == 1:
        multiline = False
      if multiline:
        depth += 1
        if stack and stack[-1].multiline and islist:  # if we are in a multiline block, we should not insert \n immediately after [
          outfile.write(b' ' * (spaces - 1))
        else:
          outfile.write(b'\n')
          outfile.write(indent * depth)

      stack.append(Context(i, flip[c], multiline, head))
      head = None
    elif c in b')]':
      outfile.write(c)
      i += 1

      s = stack.pop()
      if c != s.closer:
        raise ValueError(f"mismatched bracket: {flip[s.closer]} at byte {s.begin} closed by {c} at {i}.")
      if s.multiline:
        depth -= 1
      if not stack:
        outfile.write(b'\n')
    elif c == b'"':
      string = string_re.match(data, i)
      if not string:
        raise ValueError(f"unclosed string beginning at byte {i+1}.")
      outfile.write(string[0])
      i = string.end(0)
    else:
      sys.stderr.buffer.write(b'\npreceding text:\n')
      sys.stderr.buffer.write(data[max(0,i-100):i])
      raise ValueError(f"unsupported @ {i} = {c}")

  if stack:
    closers = ''.join(chr(x.closer[0]) for x in reversed(stack))
    log.warning(f"unclosed brackets. expected: '{closers}'. malformed adt?")


@contextlib.contextmanager
def measure_time(context: str):
  log.info(f'starting {context}', stacklevel=3)
  start = time.perf_counter()
  yield lambda: time.perf_counter() - start
  log.debug(f'... done in {time.perf_counter() - start:.3f} seconds', stacklevel=3)


def main(args):
  infile = args.input
  outfile = args.output
  update = args.update
  spaces = args.spaces

  with measure_time('read'):
    data = infile.read()
    infile.close()
    log.debug(f'    read {len(data):,} bytes')

  if len(data) > 5000000:
    log.warning(f'large input of {len(data):,} bytes. formatting may be slow.')

  outbuf = None
  if update:
    outfile = outbuf = io.BytesIO()

  with measure_time('pretty + write' if not update else 'pretty'):
    pretty(outfile, data, spaces)

  if update:
    with measure_time('write'), open(infile.name, 'wb') as outfile:
      assert outbuf
      outfile.write(outbuf.getbuffer())

if __name__ == '__main__':
  logging.basicConfig(format='[%(asctime)s %(module)s:%(lineno)-3d %(levelname)-7s] %(message)s')

  argp = argparse.ArgumentParser(description="pretty formats BAP ADT files.")
  argp.add_argument('input', nargs='?', type=argparse.FileType('rb'), default=sys.stdin.buffer,
                    help="input .adt file (default: stdin)")
  excl = argp.add_mutually_exclusive_group()
  excl.add_argument('output', nargs='?', type=argparse.FileType('wb'), default=sys.stdout.buffer,
                    help="output file name (default: stdout)")

  argp.add_argument('--spaces', '-s', default=1, type=int, 
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

