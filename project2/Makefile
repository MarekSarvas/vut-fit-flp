all:flp21-log
	./flp21-log < test.in

flp21-log: hamilton_cycle.pl
	swipl -q -g start -o flp21-log -c hamilton_cycle.pl

io: basic_io.pl
	swipl -q -g start -o io -c basic_io.pl
tut: log_tut.pl
	swipl -q -g start -o tut -c log_tut.pl
zip:
	zip -r flp-log-xsarva00.zip README.md Makefile hamilton_cycle.pl 

clean:
	rm ./flp21-log io tut 
