#!/usr/bin/env python3
# vim: ts=2 sts=2 et sw=2

import sys
import json
import dataclasses
from pathlib import Path

@dataclasses.dataclass
class Decl:
  tyname: str
  valname: str
  args: None | list[tuple[str, str]]  # list of (name, type)
  extends: list[str] = dataclasses.field(default_factory=list)

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
  assert len(d['stats']) == 1 and d['stats'][0]['type'] == 'Pkg'
  pkg = d['stats'][0]['stats']
  for defn in pkg:
    ty = defn['type']
    mods = [x['type'] for x in defn.get('mods', [])]
    name = defn.get('name', {}).get('value', None)
    # print(mods)
    # print(defn)
    if ty == 'Defn.Object' and 'Mod.Case' in mods:
      yield Decl(name + '.type', name, None)
    elif ty == 'Defn.Class':
      params = [y['name']['value'] for x in defn['ctor']['paramClauses'] for y in x['values']]
      paramtypes = [typestring(y['decltpe']) for x in defn['ctor']['paramClauses'] for y in x['values']]
      if 'Mod.Case' in mods:
        yield Decl(name, name, list(zip(params, paramtypes)))
      else:
        yield Decl(name, name, list(zip(params, paramtypes)))


def make_repr(d: Decl, fn: str = 'toScala') -> str:
  # print(d.args)
  var = 'x'
  if d.args is not None:
    argstr = '(' + ', '.join(f'${{summon[ToScala[{ty}]].{fn}({var}.{arg})}}' for arg,ty in d.args) + ')'
  else:
    argstr = ''
  return (
    f'given ToScala[{d.tyname}] with\n'
    f'  def {fn}({var}: {d.tyname}) = s"{d.valname}{argstr}"'
  )

def main(argv: list[str]):
  for f in sys.argv[1:]:
    print('// generated from', f)
    with open(f) as fp:
      for d in toplevel_definitions(json.load(fp)):
        print(make_repr(d))
    print()


if __name__ == '__main__':
  main(sys.argv)



