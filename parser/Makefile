#Author:  David Hurst
#UID:     u0457337
#Partner: Blake Sleight



all: pyparse
	@echo "compiled"

run: pyparse 
	./pyparse

lex:
	pylex

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

parse: pyparse derivative-parsers.rkt
	./pyparse

clean:
	rm -rf compiled pyparse sdiff

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: pyparse sdiff 
	for i in tests/*.py; do pylex < $$i | ./pyparse > $$i.out; ./sdiff $$i.out $$i.expected; done




#Author: David Hurst
#UID   : u045737
