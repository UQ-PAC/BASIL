for dir in */; do 
    cd $dir
    dir="${dir%/}"
    cp "$dir.c" ~/dstg/funny_gtirb_stuff/gtirb-semantics/extras/example-bin/example.c 
    cd ~/dstg/funny_gtirb_stuff/gtirb-semantics/scripts
    ./fulltest.sh 
    cp ../extras/example-bin/example.gts ~/dstg/bil-to-boogie-translator/examples/"$dir"/"$dir.gts"
    cp ../extras/example-bin/example.ast.json ~/dstg/bil-to-boogie-translator/examples/"$dir"/"$dir.ast.json"
    cd ~/dstg/bil-to-boogie-translator/examples/
done