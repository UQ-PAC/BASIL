if [ $# -ne 1 ]
  then
    echo "Usage: ./lift_docker.sh [c file to be lifted]"
    exit 1
fi

echo "WARNING: using latest docker image"

abspath=$(dirname "$(realpath $1)")
filename=$(basename -s .c $1)

aarch64-linux-gnu-gcc -fno-plt -fno-pic "$1"
# aarch64-linux-gnu-gcc "$1"

# shellcheck disable=SC2024
sudo docker run -v "$(pwd)/a.out:/a.out" binaryanalysisplatform/bap:latest bap /a.out -d > "$abspath/$filename.bil"
readelf -s -r a.out > "$abspath/$filename.elf"
rm a.out
