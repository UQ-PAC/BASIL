#!/usr/bin/env python3
# vim: ts=2 sts=2 et sw=2

import sys
import json
import dataclasses
from collections import defaultdict
from pprint import pprint

@dataclasses.dataclass
class Decl:
  tyname: str
  valname: str
  args: None | list[tuple[str, str]]  # list of (name, type)
  extends: list[str] = dataclasses.field(default_factory=list)
  istop: bool = False

Hierarchy = dict[str, 'Hierarchy | Decl']

BANNED = ['DirectCall', 'IndirectCall', 'GoTo', 'Return']

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
  traits: dict[str, Hierarchy] = {}

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
      params = [y['name']['value'] for x in defn['ctor']['paramClauses'] for y in x['values']]
      paramtypes = [typestring(y['decltpe']) for x in defn['ctor']['paramClauses'] for y in x['values']]
      if 'Mod.Case' in mods:
        decl = Decl(name, name, list(zip(params, paramtypes)))
      else:
        decl = Decl(name, name, list(zip(params, paramtypes)))

    if decl is not None:
      extends = [e for e in extends if e in traits]
      for e in extends:
        traits[e][name] = decl
      if not extends:
        h[name] = decl

  return h

def indent(generator):
  indentnext = False
  for l in generator:
    if indentnext:
      yield '  '
      indentnext = False
    yield l
    if l.endswith('\n'):
      indentnext = True

def make_repr_match_case(d: Hierarchy | Decl):
  # print(d.args)
  if isinstance(d, Decl):
    if d.valname in BANNED:
      yield f'summon[ToScala[{d.tyname}]].toScala(x)\n'
      return

    if d.args is not None:
      argstr = '(' + ', '.join(f'${{x.{arg}.toScala}}' for arg,_ in d.args) + ')'
    else:
      argstr = ''
    yield f's"{d.valname}{argstr}"\n'
    return

  yield 'x match {\n'

  for k, x in d.items():
    k = x.tyname if isinstance(x, Decl) else k
    yield f'  case x: {k} => '
    yield from indent(make_repr_match_case(x))
  yield '}\n'

def make_repr_given(h: Hierarchy):
  for k, x in h.items():
    yield f'given ToScala[{k}] with\n'
    yield f'  extension (x: {k}) def toScala: String = '
    yield from indent(make_repr_match_case(x))
    yield '\n'


def main(argv: list[str]):
  for f in argv[1:]:
    print('// generated from', f)
    with open(f) as fp:
      d = toplevel_definitions(json.load(fp))
      print(*make_repr_given(d), sep='')
    print('// end generated from', f)
    print()


if __name__ == '__main__':
  main(sys.argv)



