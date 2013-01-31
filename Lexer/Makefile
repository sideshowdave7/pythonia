# Your Lexer's executable target
LEXER_EXE_TARGET = PYTHONIA-OPTIMUS-LEXER

# Your Lexer's executable
LEXER_EXE = lexer

# Path to Racket
RACKET_PATH = /u/homes/dhurst/racket/bin/racket

# Additional (optional) files to remove for make clean
ADDITIONAL_CLEAN_FILES =

# Command to build your executable; You'll probably want to
# change the compilation command. You won't need raco unless
# you did your lexer in Racket
$(LEXER_EXE):
	raco exe lexer.rkt

# Dependencies of your target; you probably won't need to edit
# this.
$(LEXER_EXE_TARGET): $(LEXER_EXE)

# DON'T EDIT ANYTHING BELOW THIS UNLESS YOU REALLY WANT TO

SRCS = $(shell ls tests/*.py)
EXPECTED = $(SRCS:.py=.py.expected)
EXPECTED_NORMALIZED = $(SRCS:.py=.py.expected.normalized)
EXPECTED_NORMALIZED_RKT = $(SRCS:.py=.py.expected.normalized.rkt)
ACTUAL = $(SRCS:.py=.py.actual)

%.py.expected: %.py
	curl --data-urlencode "file@$<" http://matt.might.net/apps/pylex/pylex.php > $@ 2> /dev/null

%.py.expected.normalized.rkt: %.py.expected
	cat 'Normalizer.rkt' $< > $@

%.py.expected.normalized: %.py.expected.normalized.rkt
	$(RACKET_PATH) -t $< > $@

expected: $(EXPECTED_NORMALIZED)

test: $(LEXER_EXE_TARGET) expected
	for test in tests/*.py; do \
	    ./$(LEXER_EXE) < $$test > $$test.actual;  \
	    filename=`basename $$test | tr _ ' ' | sed s/\.py//`; \
	    cat 'Normalizer.rkt' $$test.actual   > $$test.actual.normalized.rkt; \
	    $(RACKET_PATH) -t $$test.actual.normalized.rkt   > $$test.actual.normalized 2> /dev/null; \
	    result=`diff -q $$test.actual.normalized $$test.expected.normalized`; \
	    if echo "$$result" |grep -q differ; then \
	      echo '\033[01;31m'FAIL ... "$$filename"'\033[00;00m'; \
	      diff $$test.actual.normalized $$test.expected.normalized; \
	    else \
	      echo '\033[01;32m'PASS ... "$$filename"'\033[00;00m'; \
	    fi  ; done

clean:
	rm -f $(LEXER_EXE) $(ADDITIONAL_CLEAN_FILES) tests/*.normalized tests/*.normalized.rkt tests/*.actual tests/*.expected
