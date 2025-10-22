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
    "after-guard-complex-VarsTooMany-count",
    "after-stmt-count"]


if (sys.argv[1] == "header"):
    print(", ".join(headers))
    exit(0)

rs = {}

for i in headers:
    rs[i] = "0"

rs["fname"] = sys.argv[1]


for line in fileinput.input(encoding='utf-8', errors='ignore', files=('-')):
    # print(line, end="")
    if ('tv-eval-marker' in line):
        line = line.split(":")[1].strip()
        rc = line.split("=")
        rs[rc[0].strip()] = rc[1].strip()




print(", ".join(rs[i] for i in headers))
