import fileinput
import sys

headers = ["fname", "before-stmt-count",
    "before-guard-total-count",
    "before-guard-simple-count",
    "before-guard-complex-unique-count",
    "before-guard-complex-VarsTooMany-count",
    "before-guard-complex-HasFlagRegisters-count",
    "after-guard-total-count",
    "after-guard-simple-count",
    "after-guard-complex-unique-count",
    "after-guard-complex-OpsTooMany-count",
    "after-guard-complex-VarsTooMany-count"]


if (sys.argv[1] == "header"):
    print(", ".join(headers))
    exit(0)

rs = {}
rs["fname"] = sys.argv[1]

for line in fileinput.input( errors='ignore'):
    if ('tv-eval-marker' in line):
        line = line.split(":")[1].strip()
        rc = line.split("=")
        rs[rc[0]] = rc[1]

print(", ".join(rs[i] for i in headers))
