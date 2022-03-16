
all:
	ghc -Wall -o ./flp21-fun src/Main 

flp21-fun:
	ghc -Wall -o ./flp21-fun src/Main 

test1: flp21-fun 
	./flp21-fun -i test/test01.in

zip:
	zip -r flp-fun-xsarva00.zip ./doc ./src ./test Makefile

clean:
	rm ./flp21-fun src/Main.hi src/Main.o
