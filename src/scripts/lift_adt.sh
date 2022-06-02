if [ $# -lt 2 ]
  then
    echo "Usage: ./lift_adt.sh [c file to be lifted] [name of adt file produced]"
    exit 1
fi
echo Lifting "$1"...
echo Compiling to "$1".out...
aarch64-linux-gnu-gcc -fno-plt -fno-pic "$1" -o "$1".out
echo Lifting to "$2"...
bap "$1".out -d adt > "$2"

echo Done