LEXER_EXE_TARGET = PYTHONIA-OPTIMUS-LEXER
LEXER_EXE = lexer
RACKET_PATH = /u/homes/dhurst/racket/bin/racket

$(LEXER_EXE):
	raco exe lexer.rkt

$(LEXER_EXE_TARGET): $(LEXER_EXE)

SRCS = $(shell ls tests/*.py)
EXPECTED = $(SRCS:.py=.py.expected)
EXPECTED_NORMALIZED = $(SRCS:.py=.py.expected.normalized)
EXPECTED_NORMALIZED_RKT = $(SRCS:.py=.py.expected.normalized.rkt)
ACTUAL = $(SRCS:.py=.py.actual)

%.py.expected: %.py
	curl --data-urlencode "file@$<" http://matt.might.net/apps/pylex/pylex.php > $@ 2> /dev/null
	#./pylex < $< > $@ 2> /dev/null

%.py.expected.normalized.rkt: %.py.expected
	cat 'Normalizer.rkt' $< > $@

%.py.expected.normalized: %.py.expected.normalized.rkt
	$(RACKET_PATH) -t $< > $@

run: $(RUN_TARGET)
	./$(LEXER_EXE)

expected: $(EXPECTED_NORMALIZED)

test: $(LEXER_EXE_TARGET) expected
	for test in tests/*.py; do \
	    ./$(LEXER_EXE) < $$test > $$test.actual;  \
	    filename=`basename $$test | tr _ ' ' | sed s/\.py//`; \
	    cat 'Normalizer.rkt' $$test.actual   > $$test.actual.normalized.rkt; \
	    $(RACKET_PATH) -t $$test.actual.normalized.rkt   > $$test.actual.normalized 2> /dev/null; \
	    result=`diff -q $$test.actual.normalized $$test.expected.normalized`; \
	    if echo "$$result" |grep -q differ; then \
	      echo FAIL ... "$$filename"; \
	      diff $$test.actual.normalized $$test.expected.normalized > ./$$test.diffs; \
	    else \
	      echo PASS ... "$$filename"; \
	    fi  ; done

clean:
	rm -f $(LEXER_EXE) tests/*.normalized tests/*.normalized.rkt tests/*.actual tests/*.expected diffs
