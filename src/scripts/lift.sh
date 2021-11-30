
if [ $# -lt 2 ]
  then
    echo "Usage: ./lift.sh [c file to be lifted] [name of bil file produced]"
    exit 1
fi
echo Lifting "$1"...
echo Compiling to a.out...
aarch64-linux-gnu-gcc -fno-plt -fno-pic "$1"
echo Lifting to "$2"...
sudo docker run binaryanalysisplatform/bap:latest bap a.out -d > "$2"
echo Deleting a.out
rm a.out
echo Done

