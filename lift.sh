set -e
if [ $# -lt 3 ]
  then
    echo "Usage: ./lift.sh [c file to be lifted] [name of adt file produced] [name of relf file produced]"
    exit 1
fi
aarch64-linux-gnu-gcc -fno-plt -fno-pic -fno-stack-protector "$1" -o "$1.out"
bap "$1".out -d adt:"$2"
readelf -r -s -W "$1.out" > "$3"
#rm "$1".out
