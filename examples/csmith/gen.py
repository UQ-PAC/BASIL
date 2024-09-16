import subprocess


fn_min = 10
fn_max = 11
block_min = 4
block_max = 8 
seed_min = 0
num_rand = 50 

# produces programs that we can't handle
# cmd = ["csmith", "--seed", str(seed), "--max-block-size", str(max_blocksize), "--max-funcs", str(max_funcs), "--output", f"{fname}.c"]

csmith_safe = "--no-arrays --no-bitfields --no-checksum --no-comma-operators --no-longlong --no-int8 --no-uint8 --no-float --no-math64 --no-inline-function --no-safe-math --no-packed-struct --no-pointers --no-structs --no-unions --no-volatile-pointers --no-const-pointers".split(" ")

files = []
vals = {}

for globals in ["--no-global-variables", "--global-variables"]:
    for max_funcs in range(fn_min, fn_max):
        for seed in range(seed_min, seed_min + num_rand):
            for max_blocksize in range(block_min, block_max):
                fname = f"c-{max_funcs}-{max_blocksize}-{seed}-{globals.replace('-', '')}"
                files.append(fname)
                vals[fname] = {
                    'globals' : globals,
                    'max_funcs': max_funcs,
                    'seed' : seed,
                    'max_blocksize': max_blocksize
                }


def csmith():
    for fname in files:
        globals = vals[fname]['globals']
        seed = vals[fname]['seed']
        max_funcs = vals[fname]['max_funcs']
        max_blocksize = vals[fname]['max_blocksize']
        cmd = ["csmith", "--max-block-size", str(max_blocksize), "--max-funcs", str(max_funcs), "--output", f"{fname}.c"] + csmith_safe
        print(cmd)
        gen = subprocess.run(cmd)
        checksum_cmd = f"gcc {fname}.c -o ${fname}-native.out && ./{fname}.out && rm {fname}-native.out"


def checksum():
    for fname in files:
        print(fname)
        checksum_cmd = f"clang {fname}.c -w -o {fname}-native.out -I /usr/include/csmith && ./{fname}-native.out && rm {fname}-native.out"
        print(checksum_cmd)
        try:
            checksum = subprocess.run(checksum_cmd, capture_output=True, text=True, shell=True, timeout=10)
            print("checksum" , checksum.stdout, checksum.stderr)
            with open(fname + ".checksum", 'w') as f:
                f.write(checksum.stdout)
        except Exception as e:
            print(e)

def lift():
    for fname in files:
        compilecmd = ['clang-15', '-w', '-target', 'aarch64-linux-gnu', fname + ".c", '-I', '/usr/include/csmith', '-o', fname + '.out']
        print(compilecmd)
        compile = subprocess.run(compilecmd)
        cmd =  ["bap", f"{fname}.out", "-d", "adt:" + fname + ".adt", "-d", "bir:"+  fname + ".bir" ]
        relfcmd = f"readelf -s -r -w {fname}.out > {fname}.relf" 
        print(cmd)
        subprocess.run(cmd)
        print(relfcmd)
        subprocess.run(relfcmd, shell=True)

csmith()
lift()
