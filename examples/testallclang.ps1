$folders = Get-ChildItem -Name -Directory
foreach ($name in $folders) {
    cd $name
    $adt = $name + "_clang.adt"
    $relf = $name + "_clang.relf"
    $spec = $name + ".spec"
    $bpl = $name + "_clang.bpl"
    $result = $name + "_clang_result.txt"
    $err = $name +"_clang_error.txt"
    $args = 
    if (Test-Path "$spec") {
        $proc = Start-Process java -NoNewWindow -PassThru -ArgumentList "-jar","../../target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar","$adt","$relf","$spec","$bpl" -RedirectStandardError $err 
        $proc | Wait-Process
    } else {
        $proc = Start-Process java -NoNewWindow -PassThru -ArgumentList "-jar","../../target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar","$adt","$relf","$bpl" -RedirectStandardError $err
        $proc | Wait-Process
    }
    Start-Process boogie -NoNewWindow $bpl -RedirectStandardOutput $result
    echo "$name done"
    cd ../
}