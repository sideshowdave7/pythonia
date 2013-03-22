all: pylex pyparse pytrans
	@echo "compiled"

pylex: pylex.rkt
	raco exe pylex.rkt

pyparse: pyparse.rkt python-ast.grm.sx
	raco exe pyparse.rkt

pytrans: pytransylvania.rkt derivative-parsers.rkt
	raco exe pytransylvania.rkt

lex: pylex
	@./pylex

parse: pyparse
	@./pyparse

trans: pytrans
	@./pytransylvania

part: pylex pyparse
	@./pylex | ./pyparse

run: pylex pyparse pytrans
	@./pylex | ./pyparse | ./pytransylvania

clean:
	rm -rf compiled pyparse sdiff pylex pytransylvania
	rm -f tests/*.py.out*
	rm -f tests/*.py.expected*

sdiff: sdiff.rkt
	raco exe sdiff.rkt

atest: pylex pyparse pytrans sdiff
	make sexptest

test: pylex pyparse pytrans sdiff
	for i in tests/*.py; do printf "%s\n" "Testing: $$i"; ./pylex < $$i | ./pyparse | ./pytransylvania > $$i.out; cat hir-header.rkt $$i.expected > $$i.expected.rkt | cat hir-header.rkt $$i.out > $$i.out.rkt; racket $$i.expected.rkt > $$i.expected.txt; racket $$i.out.rkt > $$i.out.txt; diff -q $$i.expected.txt $$i.out.txt; done

answers:
	for i in tests/*.py; do if [ ! -f $$i.expected ]; then printf "%s\n" "Copying $$i"; scp $$i caprica:temp.py; ssh caprica "pylex < temp.py | pyparse | pytransylvania > temp.py.expected"; scp caprica:temp.py.expected $$i.expected; fi done

sextest: pylex pyparse pytrans sdiff
	for i in tests/*.py; do ./pylex < $$i | ./pyparse | ./pytransylvania > $$i.out; ./sdiff $$i.out $$i.expected; done
