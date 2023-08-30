if [ $# -lt 3 ]; then
  echo "Usage: ./run.sh *.adt *.relf *.spec [output.bpl]"
  exit 1
fi
if [ "$4" ]; then
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar $1 $2 $3 $4
else
  java -jar target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar $1 $2 $3
fi
