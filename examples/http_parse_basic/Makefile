
result: example.bpl extraspec.bpl
	boogie example.bpl extraspec.bpl /mv example.model /proverLog lambdas.smt2 /smoke | tee resultlambdas
	boogie example-nolambda.bpl extraspec.bpl /useArrayAxioms /mv example.model /smoke | tee resultaxioms
	boogie example-nolambda.bpl extraspec.bpl /mv example.model | tee result

a.out: example.c
	aarch64-linux-gnu-gcc example.c

example.adt: a.out
	bap-aslp a.out -d adt:example.adt
	bap-aslp a.out -d > example.bil
	readelf -s -r -W a.out > example.relf

example.bpl: example.adt example.spec
	java -jar ../../target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar --adt example.adt --relf example.relf -o example.bpl --spec example.spec --boogie-use-lambda-stores
	java -jar ../../target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar --adt example.adt --relf example.relf -o example-nolambda.bpl --spec example.spec 


