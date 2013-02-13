all: pyparse
	@echo "compiled"

lex:
	@echo "insert code to run lexer here"

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

parse: pyparse derivative-parsers.rkt
	@pylex | ./pyparse

clean:
	rm -rf compiled pyparse sdiff
	@echo "insert code to clean here"

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: pyparse sdiff
	for i in tests/*.py; do make parse < $$i > $$i.out; ./sdiff $$i.out $$i.expected; done
