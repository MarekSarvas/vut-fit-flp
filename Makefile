
all:
	ghc -Wall -o ./flp21-fun src/Main 

flp21-fun: src/Main.hs
	ghc -Wall -o ./flp21-fun src/Main 

testi: flp21-fun 
	./test.sh i	
test1: flp21-fun 
	./test.sh 1	
test2: flp21-fun 
	./test.sh 2	
teste: flp21-fun 
	./test.sh e	

stdin: flp21-fun
	./flp21-fun -i 

zip:
	zip -r flp-fun-xsarva00.zip ./doc ./src ./test Makefile test.sh

clean:
	rm ./flp21-fun src/Main.hi src/Main.o
