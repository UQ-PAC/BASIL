for dir in */; do 
    cd $dir
    dir="${dir%/}"
    aarch64-linux-gnu-gcc -fno-plt -fno-pic "$dir.c" -o "$dir.out"
    bap "$dir.out" -d adt:"$dir.adt" -d bir:"$dir.bir" --primus-lisp-semantics=disable \
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
    readelf -s -r  -W "$dir.out" > "$dir.relf"
    echo "done $dir"
    cd ../
done