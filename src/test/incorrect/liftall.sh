for dir in */; do 
    cd "$dir"
    name="${dir%/}"
    mkdir -p clang
    clang-15 "$name.c" -o "clang/$name.out" -target aarch64-linux-gnu
    bap "clang/$name.out" -d adt:"clang/$name.adt" -d bir:"clang/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "clang/$name.out" > "clang/$name.relf"
    if [ ! -f "clang/$name.out" ]; then
        rm -r -d clang
    fi
    mkdir -p clang_no_plt_no_pic
    clang-15 -fno-plt -fno-pic "$name.c" -o "clang_no_plt_no_pic/$name.out" -target aarch64-linux-gnu
    bap "clang_no_plt_no_pic/$name.out" -d adt:"clang_no_plt_no_pic/$name.adt" -d bir:"clang_no_plt_no_pic/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "clang_no_plt_no_pic/$name.out" > "clang_no_plt_no_pic/$name.relf"
    if [ ! -f "clang_no_plt_no_pic/$name.out" ]; then
        rm -r -d clang_no_plt_no_pic
    fi
    mkdir -p clang_O2
    clang-15 -O2 "$name.c" -o "clang_O2/$name.out" -target aarch64-linux-gnu
    bap "clang_O2/$name.out" -d adt:"clang_O2/$name.adt" -d bir:"clang_O2/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "clang_O2/$name.out" > "clang_O2/$name.relf"
    if [ ! -f "clang_O2/$name.out" ]; then
        rm -r -d clang_O2
    fi
    mkdir -p clang_pic
    clang-15 -fPIC "$name.c" -o "clang_pic/$name.out" -target aarch64-linux-gnu
    bap "clang_pic/$name.out" -d adt:"clang_pic/$name.adt" -d bir:"clang_pic/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "clang_pic/$name.out" > "clang_pic/$name.relf"
    if [ ! -f "clang_pic/$name.out" ]; then
        rm -r -d clang_pic
    fi
    mkdir -p gcc
    aarch64-linux-gnu-gcc "$name.c" -o "gcc/$name.out"
    bap "gcc/$name.out" -d adt:"gcc/$name.adt" -d bir:"gcc/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "gcc/$name.out" > "gcc/$name.relf"
    if [ ! -f "gcc/$name.out" ]; then
        rm -r -d gcc
    fi
    mkdir -p gcc_no_plt_no_pic
    aarch64-linux-gnu-gcc -fno-plt -fno-pic "$name.c" -o "gcc_no_plt_no_pic/$name.out"
    bap "gcc_no_plt_no_pic/$name.out" -d adt:"gcc_no_plt_no_pic/$name.adt" -d bir:"gcc_no_plt_no_pic/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "gcc_no_plt_no_pic/$name.out" > "gcc_no_plt_no_pic/$name.relf"
    if [ ! -f "gcc_no_plt_no_pic/$name.out" ]; then
        rm -r -d gcc_no_plt_no_pic
    fi
    mkdir -p gcc_O2
    aarch64-linux-gnu-gcc -O2 "$name.c" -o "gcc_O2/$name.out"
    bap "gcc_O2/$name.out" -d adt:"gcc_O2/$name.adt" -d bir:"gcc_O2/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "gcc_O2/$name.out" > "gcc_O2/$name.relf"
    if [ ! -f "gcc_O2/$name.out" ]; then
        rm -r -d gcc_O2
    fi
    mkdir -p gcc_pic
    aarch64-linux-gnu-gcc -fPIC "$name.c" -o "gcc_pic/$name.out"
    bap "gcc_pic/$name.out" -d adt:"gcc_pic/$name.adt" -d bir:"gcc_pic/$name.bir" --primus-lisp-semantics=disable
    readelf -s -r -W "gcc_pic/$name.out" > "gcc_pic/$name.relf"
    if [ ! -f "gcc_pic/$name.out" ]; then
        rm -r -d gcc_pic
    fi
    echo "done $name"
    cd ../
done