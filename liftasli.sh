if [ $# -lt 3 ]
  then
    echo "Usage: ./lift.sh [c file to be lifted] [name of adt file produced] [name of relf file produced]"
    exit 1
fi
aarch64-linux-gnu-gcc -fno-plt -fno-pic -fno-stack-protector "$1"
bap "$1".out -d adt:"$2" --primus-lisp-semantics=disable \
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
readelf -r -s -W "$1".out > "$3"
#rm "$1".out
