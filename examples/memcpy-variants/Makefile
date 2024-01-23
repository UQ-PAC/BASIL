
.PHONY=all
.PHONY=result
.PHONY=resultiter

all: resultiter result

result: example.bpl extraspec.bpl iterator-spec.bpl
	boogie example.bpl extraspec.bpl /mv example.model /smoke /proverLog result.log | tee result

resultiter: example.bpl extraspec.bpl iterator-spec.bpl
	boogie iterator-spec.bpl extraspec.bpl /mv example.model /smoke /vcsSplitOnEveryAssert /proverLog iterator.log | tee resultiter

a.out: example.c
	aarch64-linux-gnu-gcc example.c -fno-builtin-memcpy

example.adt: a.out
	bap-aslp a.out -d adt:example.adt
	bap-aslp a.out -d > example.bil
	readelf -s -r -W a.out > example.relf

example.bpl: example.adt example.spec
	java -jar ../../target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar --adt example.adt --relf example.relf -o example.bpl --spec example.spec --boogie-use-lambda-stores --dump-il example.il


iterator-spec.bpl: example.adt memcpyspec-iterindex.spec
	java -jar ../../target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar --adt example.adt --relf example.relf -o iterator-spec.bpl --spec memcpyspec-iterindex.spec --boogie-use-lambda-stores

