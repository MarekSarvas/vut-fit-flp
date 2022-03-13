
all:
	ghc -o ./flp21-fun src/Main 

test1:
	./flp21-fun -2 test/test01.in

zip:
	zip -r flp-fun-xsarva00.zip ./doc ./src ./test Makefile

clean:
	rm ./flp21-fun src/Main.hi src/Main.o
