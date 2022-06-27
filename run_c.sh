if [ $# -lt 1 ]
then
  echo "Usage: ./run_c.sh *.c [output.bpl]"
  exit 1
fi
aarch64-linux-gnu-gcc -fno-plt -fno-pic "$1" -o "$1.out"
bap "$1".out -d adt:"$1.adt"
readelf "$1".out -r -s > "$1.relf"

if [ "$2" ]
then
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar "$1.adt" "$1.relf" $2
else
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar "$1.adt" "$1.relf"
fi