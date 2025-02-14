#!/usr/bin/env python3
# vim: ts=2 sts=2 et sw=2

import sys
import json
import shlex
import tempfile
import dataclasses
from collections import defaultdict
from pprint import pprint

"""
Generates ToScala instances for hierarchies of case classes.
See ToScala.scala for the definition of the trait and ToScalaGenerated.scala
for more information about the generated code.

usage:

  make_repr_functions.py <SCALA_TEMPLATE> [JSON_FILES...]

"""

@dataclasses.dataclass
class Decl:
  tyname: str
  valname: str
  args: None | list[tuple[str, str]]  # list of (name, type)
  hasprivate: bool = False

Hierarchy = dict[str, 'Hierarchy | Decl']

def new_hierarchy() -> Hierarchy:
  return defaultdict(new_hierarchy)

def name(x) -> str:
  assert x['type'] == 'Term.Name'
  return x['value']

def typestring(x) -> str:
  if x['type'] in ('Type.Name', 'Term.Name'):
    return x['value']
  elif x['type'] == 'Type.Select':
    return typestring(x['qual']) + '.' + typestring(x['name'])
  elif x['type'] == 'Type.Apply':
    return typestring(x['tpe']) + '[' + ', '.join(typestring(x) for x in x['argClause']['values']) + ']'
  assert False, "unsupported type type: " + str(x)


def toplevel_definitions(d: dict):
  h = new_hierarchy()
  traits: dict[str, Hierarchy | Decl] = {}

  assert len(d['stats']) == 1 and d['stats'][0]['type'] == 'Pkg'
  pkg = d['stats'][0]['stats']
  for defn in pkg:
    ty = defn['type']
    mods = [x['type'] for x in defn.get('mods', [])]
    name = defn.get('name', {}).get('value', None)
    try:
      extends = [typestring(x['tpe']) for x in defn['templ']['inits']]
    except KeyError:
      extends = []
    # print(name, extends)
    # print(defn)
    decl = None
    if ty == 'Defn.Trait':
      assert name
      traits[name] = traits.get(name, new_hierarchy())
      decl = traits[name]
    elif ty == 'Defn.Object' and 'Mod.Case' in mods:
      decl = Decl(name + '.type', name, None)
    elif ty == 'Defn.Class':
      paramclauses = [y for x in defn['ctor']['paramClauses'] for y in x['values']]
      params = [y['name']['value'] for y in paramclauses]
      paramtypes = [typestring(y['decltpe']) for y in paramclauses]
      hasprivate = any('Mod.Private' == m['type'] for x in paramclauses for m in x['mods'])
      if 'Mod.Case' in mods:
        decl = Decl(name, name, list(zip(params, paramtypes)), hasprivate)
      else:
        decl = Decl(name, name, list(zip(params, paramtypes)), hasprivate)

    if decl and isinstance(decl, Decl):
      traits[decl.tyname] = decl

    if decl is not None:
      extends = [e for e in extends if e in traits]
      for e in extends:
        t = traits[e]
        assert isinstance(t, dict), "extending a concrete class??"
        t[name] = decl
      if not extends:
        h[name] = decl

  # pprint(traits)
  return h, traits

def indent(generator):
  indentnext = False
  for l in generator:
    if indentnext:
      yield '  '
      indentnext = False
    yield l
    if l.endswith('\n'):
      indentnext = True

def make_repr_match_case(d: Hierarchy | Decl, externals):
  # print(d.args)
  if isinstance(d, Decl):

    if d.args is not None:
      argstr = '(' + ', '.join(f'${{x.{arg}.toScala}}' for arg,_ in d.args) + ')'
      valstr = '(' + ', '.join(f'x.{arg}' for arg,_ in d.args) + ')'
    else:
      argstr = ''
      valstr = ''

    if d.valname in externals:
      summon = f'summon[ToScala[{d.tyname}]].toScala(x)'
      expr = 'if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else ' + summon + '\n'
    else:
      expr = f's"{d.valname}{argstr}"\n'

    valcheck = [f'def ensure_constructible(): {d.tyname} = {d.valname}{valstr}\n']
    if d.hasprivate:
      valcheck = ['// unable to validate constructor with private field:\n'] + ['// '+x for x in valcheck]

    yield '{'
    yield from indent(['\n', *valcheck, expr])
    yield '}\n'
    return

  yield 'x match {\n'

  for k, x in d.items():
    k = x.tyname if isinstance(x, Decl) else k
    yield f'  case x: {k} => '
    yield from indent(make_repr_match_case(x, externals))
  yield '}\n'

def make_repr_given(h: Hierarchy, externals):
  for k, x in h.items():
    yield f'given ToScala[{k}] with\n'
    yield f'  extension (x: {k}) def toScala: String = '
    yield from indent(make_repr_match_case(x, externals))
    yield '\n'

SCISSORS_TOKEN = '--- >8 ---'
EXTERNALS_TOKEN = 'Externals:'

def main(argv: list[str]):
  infile, *jsonfiles = argv[1:]

  with tempfile.TemporaryFile('w+') as out:

    externals = set()
    with open(infile) as inf:
      for l in inf:
        out.writelines([l])
        if EXTERNALS_TOKEN in l:
          right = l.split(EXTERNALS_TOKEN, 1)[-1]
          externals |= set(x.strip() for x in right.split(','))
        if SCISSORS_TOKEN in l:
          break

    print(file=out)
    print('// format: off', file=out)
    print(file=out)
    print('// command:\n//', *(shlex.quote(x) for x in argv), file=out)
    print(file=out)

    for f in jsonfiles:
      print('// generated from', f, file=out)
      with open(f) as fp:
        d, traits = toplevel_definitions(json.load(fp))
        for x in make_repr_given(d, externals):
          print(x, end='', file=out)
        externals -= set(traits)
      print('// end generated from', f, file=out)
      print(file=out)

    print('// format: on', file=out)
    out.seek(0)

    assert externals == set(), f'externals declared but not generated: {externals}'

    with open(infile, 'w') as inf:
      for l in out:
        inf.write(l)

if __name__ == '__main__':
  main(sys.argv)



