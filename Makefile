
all:
	ghc -Wall -o ./flp21-fun src/Main 

flp21-fun: src/Main.hs
	ghc -Wall -o ./flp21-fun src/Main 

test1: flp21-fun 
	./flp21-fun -i test/test01.in

stdin: flp21-fun
	./flp21-fun -i 

zip:
	zip -r flp-fun-xsarva00.zip ./doc ./src ./test Makefile

clean:
	rm ./flp21-fun src/Main.hi src/Main.o
