all: pyparse
	@echo "compiled"

run: pyparse 
	@./pylex | ./pyparse
lex:
	./pylex

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

parse: pyparse derivative-parsers.rkt
	@./pyparse

SRCS     = $(shell ls tests/*.py.lexed)
EXPECTED = $(SRCS:.py.lexed=.py.expected)

expected: $(EXPECTED)

%.py.expected: %.py.lexed
	curl --data-urlencode "file@$<" http://matt.might.net/apps/pyparse/pyparse.php > $@ 2> /dev/null


clean:
	rm -rf compiled pyparse sdiff
	@echo "insert code to clean here"

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: pyparse sdiff expected
	for i in tests/*.py; do ./pylex < $$i | ./pyparse > $$i.out; ./sdiff $$i.out $$i.expected; done
