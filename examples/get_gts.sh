for dir in */; do 
    cd $dir
    dir="${dir%/}"
    cp "$dir.c" ~/dstg/funny_gtirb_stuff/gtrib_semantics/extras/example_bin/example.c 
    cd ~/dstg/funny_gtirb_stuff/gtrib_semantics/scripts
    ./fulltest.sh 
    cp extras/example_bin/example.gts ~/dstg/bil-to-boogie-translator/examples/"$dir"/"$dir.gts"
    cp extras/example_bin/example.ast.json ~/dstg/bil-to-boogie-translator/examples/"$dir"/"$dir.ast.json"
    cd ~/dstg/bil-to-boogie-translator/examples/