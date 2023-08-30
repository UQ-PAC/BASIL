for dir in */; do 
    cd $dir
    dir="${dir%/}"
    clang-15 -fno-plt -fno-pic "$dir.c" -o "${dir}_clang.out" -target aarch64-linux-gnu
    bap "${dir}_clang.out" -d adt:"${dir}_clang.adt" -d bir:"${dir}_clang.bir" --primus-lisp-semantics=disable \
             --asli-prelude=$ASLI_PATH/prelude.asl \
             --asli-specs=$ASLI_PATH/mra_tools/arch/regs.asl \
             --asli-specs=$ASLI_PATH/mra_tools/types.asl \
             --asli-specs=$ASLI_PATH/mra_tools/arch/arch.asl \
             --asli-specs=$ASLI_PATH/mra_tools/arch/arch_instrs.asl \
             --asli-specs=$ASLI_PATH/mra_tools/arch/arch_decode.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/aes.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/barriers.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/debug.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/feature.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/hints.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/interrupts.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/memory.asl \
             --asli-specs=$ASLI_PATH/mra_tools/support/stubs.asl \
             --asli-specs=$ASLI_PATH/tests/override.asl
    readelf -s -r  -W "${dir}_clang.out" > "${dir}_clang.relf"
    echo "done $dir"
    cd ../
done