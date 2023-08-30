cd "$1"
name="$1"
bap "$name.out" -d adt:"$name.adt" -d bir:"$name.bir" --primus-lisp-semantics=disable
readelf -s -r -W "$name.out" > "$name.relf"