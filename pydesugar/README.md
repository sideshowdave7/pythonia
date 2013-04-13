David Hurst   -    James Hulse

Project 4: CPS Transformation
Spring 2013, University of Utah
CS 5460 (Compilers)

Our code can be run by typing the following:

cd /home/unid/pycps
    make 
    make run < test1.py > test1.py.cps
    make run < test2.py > test2.py.cps
    make run < test3.py > test3.py.cps
    pylex < test1.py | pyparse | pytrans | make cps > test1.py.cps
    pylex < test2.py | pyparse | pytrans | make cps > test2.py.cps
    pylex < test3.py | pyparse | pytrans | make cps > test3.py.cps






pydesugar.rkt        Desugaring
pycps.rkt	     Continuation-Passing 
tests	             Tests folder	


