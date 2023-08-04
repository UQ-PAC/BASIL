set -e
if [ $# -lt 3 ]
  then
    echo "Usage: ./lift_adt.sh [c file to be lifted] [name of adt file produced] [name of relf file produced]"
    exit 1
fi
echo Lifting "$1"...
echo Compiling
aarch64-linux-gnu-gcc -fno-plt -fno-pic -fno-stack-protector "$1" -o "$1".out
echo Lifting to "$2"...
bap "$1".out -d adt > "$2"
echo Outputting "$3"...
readelf -s -r -W "$1".out > "$3"
rm "$1".out
echo Done
