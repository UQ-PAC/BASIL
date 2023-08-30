cd $1
aarch64-linux-gnu-gcc -O2 "$1.c" -o "$1.out"
bap "$1.out" -d adt:"$1.adt" -d bir:"$1.bir" --primus-lisp-semantics=disable \
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
readelf -s -r  -W "$1.out" > "$1.relf"
echo "done $1"
cd ../