set -e
if [ $# -lt 1 ]; then
  echo "Usage: ./run_c.sh *.out [output.bpl]"
  exit 1
fi
bap "$1" -d adt:"$1.adt"
readelf "$1" -r -s -W > "$1.relf"

if [ "$2" ]; then
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar "$1.adt" "$1.relf" $2
else
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar "$1.adt" "$1.relf"
fi
