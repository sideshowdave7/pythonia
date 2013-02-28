Univeristy of Utah
Spring 2013 
CS 5460 (Compilers)
February 27, 2013

Project 2: Python Parser

Authors: David Hurst
         Blake Sleight


Table of contents:
        1. Makefile usage
        2. Parser Implementation and Behavior



1. Makefile usage:

Building the executable (should be done first):
   
      make

Running from STDIN to STDOUT:
     
      make run

Lexing from STDIN:
	
      make lex

Parsing from STDIN:

      make parser

Running test suite:

      make test

Cleaning up:

      make clean



2. Parser Implementation and Behavior:
	
	Our Parser uses exclusively the match operator to correctly parse lexed tokens. Using the provided gramer, the derp reductions reduce the lexed tokens to meet 	the specifications. Many reductions used nested match operations to produced the desired outcome. Implementation required we start from the ground and work our way up. The ground, in this case, being an atom. We then worked up slowly from the atom to expressions to tests, until everything was working together. After a few iterations everything came together nicely.
                   




        
