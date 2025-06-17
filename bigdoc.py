#!/usr/bin/env python3

# vim: sts=2 ts=2 sw=2 et

import subprocess
import tempfile
import zipfile
import json
import os
import shutil

from pathlib import Path

tmp = Path('./asd')
tmp.mkdir(exist_ok=True)
shutil.rmtree(tmp)
tmp.mkdir(exist_ok=True)

d = json.loads(subprocess.check_output(["./mill", "--ticker", "false", "show", "bigdocs.compileClasspath"]))
jars = ([Path(x.split(':')[-1]) for x in d if x.endswith('-javadoc.jar')])

searchpath = tmp / "searchData.js"
with open(searchpath, 'w') as f:
  f.write('pages = [\n')

for i, jar in enumerate(jars):
  jdir = (tmp / jar.stem)
  jdir.mkdir()

  with zipfile.ZipFile(jar, 'r') as z:
    z.extractall(jdir)
  if not (os.path.exists(jdir / "scripts/searchData.js")):
    continue
  print(jdir)
  with open(jdir / "scripts/searchData.js") as f:
    thesepages = f.read().replace('pages = [', '', 1).rstrip().rstrip('];')
  with open(searchpath, 'a') as f:
    f.write(thesepages)
    f.write(',\n')
    f.write('\n')
with open(searchpath, 'a') as f:
  f.write(']\n')

os.chdir(tmp)

subprocess.check_call("find . -type f -exec sed -i -e s@scripts/searchData.js@../searchData.js@g {} ;".split())


