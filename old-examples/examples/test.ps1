$name = $args[0]
cd $name
$adt = $name + ".adt"
$relf = $name + ".relf"
$spec = $name + ".spec"
$bpl = $name + ".bpl"
$result = $name + "_result.txt"
$err = $name +"_error.txt"
$args = 
if (Test-Path "$spec") {
    $proc = Start-Process java -NoNewWindow -PassThru -ArgumentList "-jar","../../target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar","$adt","$relf","$spec","$bpl" -RedirectStandardError $err 
    $proc | Wait-Process
} else {
    $proc = Start-Process java -NoNewWindow -PassThru -ArgumentList "-jar","../../target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar","$adt","$relf","$bpl" -RedirectStandardError $err
    $proc | Wait-Process
}
Start-Process boogie -NoNewWindow $bpl -RedirectStandardOutput $result
cd ../