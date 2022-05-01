all:flp21-log


flp21-log: hamilton_cycle.pl
	swipl -q -g start -o flp21-log -c hamilton_cycle.pl

run:
	./flp21-log < tests/test.in

zip:
	zip -r flp-log-xsarva00.zip README.md Makefile hamilton_cycle.pl tests/ 

clean:
	rm ./flp21-log
