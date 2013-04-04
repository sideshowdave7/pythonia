PYLEX=pylex
PYPARSE=pyparse
PYTRANS=pytrans
PYDESUGAR=./pydesugar
PYCPS=./pycps

PYTHON=python
RACKET=racket

all: pycps pydesugar 


desugar: pydesugar
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR)

cps: pydesugar pycps 
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS)


pycps: pycps.rkt
	raco exe pycps.rkt

pydesugar: pydesugar.rkt
	raco exe pydesugar.rkt


tests/%.lxd: tests/%.py
	$(PYLEX) < $< > $@

tests/%.ast: tests/%.lxd
	$(PYPARSE) < $< > $@

tests/%.hir: tests/%.ast
	$(PYTRANS) < $< > $@

tests/%.lir: tests/%.hir pydesugar
	$(PYDESUGAR) < $< > $@

tests/%.cps: tests/%.lir pycps
	$(PYCPS) < $< > $@

tests/%.lrkt: tests/%.lir lir-header.rkt
	cat lir-header.rkt $< > $@

tests/%.crkt: tests/%.cps cps-header.rkt
	cat cps-header.rkt $< > $@

tests/%.expected: tests/%.py
	$(PYTHON) $< > $@


tests/%.lresult: tests/%.lrkt
	$(RACKET) $< > $@

tests/%.cresult: tests/%.crkt
	$(RACKET) $< > $@


tests/%.ldiff: tests/%.lresult tests/%.expected
	@echo "diffing " $(basename $@)
	diff $(basename $@).expected $(basename $@).lresult >> $@


tests/%.cdiff: tests/%.cresult tests/%.expected
	@echo "diffing " $(basename $@)
	diff $(basename $@).expected $(basename $@).cresult >> $@



cps-tests: tests/1.cps tests/2.cps tests/3.cps tests/4.cps tests/5.cps tests/6.cps tests/7.cps tests/8.cps tests/9.cps tests/10.cps tests/11.cps tests/12.cps tests/13.cps

lir-tests: tests/1.lir tests/2.lir tests/3.lir tests/4.lir tests/5.lir tests/6.lir tests/7.lir tests/8.lir tests/9.lir tests/10.lir tests/11.lir tests/12.lir tests/13.lir


test-cps: tests/1.cdiff tests/2.cdiff tests/3.cdiff tests/4.cdiff tests/5.cdiff tests/6.cdiff tests/7.cdiff tests/8.cdiff tests/9.cdiff tests/10.cdiff tests/11.cdiff tests/12.cdiff tests/13.cdiff

test-lir: tests/1.ldiff tests/2.ldiff tests/3.ldiff tests/4.ldiff tests/5.ldiff tests/6.ldiff tests/7.ldiff tests/8.ldiff tests/9.ldiff tests/10.ldiff tests/11.ldiff tests/12.ldiff tests/13.ldiff






clean:
	rm -rf stub
	rm -rf compiled
	rm -f pydesugar pycps pycps_rkt.zo pydesugar_rkt.zo
	rm -f tests/*.{lxd,ast,lir,hir,crkt,lrkt,cdiff,ldiff,expected,cresult,lresult} stub.tar.gz
