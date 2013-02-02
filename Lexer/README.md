Univeristy of Utah
Spring 2013 
CS 5460 (Compilers)
February 1, 2013

Project 1: Python Lexer

Authors: David Hurst
         Blake Sleight


Table of contents:
        1. Makefile usage
        2. Expected Behaviour
        3. Included test notes
        4. Lexer Implementation Notes



1. Makefile usage:

Building the executable (should be done first):
   
      make

Running from STDIN to STDOUT:
     
      make run

Running test suite:

      make test

Cleaning up:

      make clean



2. Lexer Behaviour:

       Tabs:             The lexer handles tabs by assuming they are an error, and an error token is immediately generated stating so.
       Unicode:          The lexer currently does not have support for input of unicode.
       
       LITerals:         Octal and hex escaped values, e.g. '\100' will be displayed as such, and not '@' if found in a string or byte literal.  (Passes eq? inside racket except for '\000')                
                         Number literals including floating point, integer, binary, octal, hexadecimal, and imaginary numbers are processed into racket-compatible values.  
                         String and byte literals are both treated the same (both become string objects in racket, although this behaviour could be easily modified for convenience in creating the parser).
                         String and byte literals are allowed to have both raw, and non-raw versions (raw when preceded by r) according to the python lexical specification.  The lexer handles raw strings appropiately by treating the backslash "\" as a charactar and not an escape sequence.
                         If an eof is found inside a string literal, and error token is generated stating so. 

       INDENTS/DEDENTS:  All indents and dedents should behave as expected with one exception: 
                                    An indent will be generated in the case of a line explicity joined with a following line with whitespace before another token.  Pylex does not perform this way (not sure if this within Pythons lexical specification as the document is ambigious.
                     
                         Before and ENDMARKER, the correct amount of DEDENTS will be shown to reset the indentation stack to zero.
                         If the first token is indented by any amount, an INDENT will be generated.  This is not allowed in the parser, but is allowed lexically and matches with the behaviour of pylex.
                         Blank lines will not generate excess NEWLINE tokens.

       ENDMARKER:        A single ENDMARKER token is generated when the lexer encounters eof.
       KEYWORD:          A KEYWORD is generated when an ID matches with a python-defined keyword.
       PUNCT:            A PUNCT keyword is generated when the lexer encounters an python-defined OPERATOR or DELIMETER is found.
       
       Comments:         Comments are completely ignored, including when comments end in a backslash "\".
 
       Backslash:        The backslash "\", is treated differently depending on its context, according Python lexical specification.  It is used as an explicit line join outside of raw strings and comments.  Otherwise it joins two physical lines.  No newline is generated in the case of an explicit line join.

       Invalid chars:    Misplaced charactars such as "?" "`" "$" etc. generate an ERROR token.

       Noted quirks:     All known quirks with running the lexer are discussed in the testing section below.


3. Included test notes

         When the included test suite is run, several files are generated for each python file in the tests subdirectory:
  
            test.py.actual                      actual results using this lexer
            test.py.acutal.normalized           normalized results using this lexer
            test.py.expected                    expected results obtained from reference lexer
            test.py.expected.normalized         normalized expected results obtained from reference lexer
            test.py.diffs                       diff between test.py.actual.normalized and test.py.expected.normalized (no file generated if no diffs)

         The following tests produce a fail, but most are actually passes (a simple diff is not enought to check equality).  Their behaviour is explained below:

                Any test with   "error_"         as a prefix: due to the fact that this lexer generates error tokens differently (specification only requires ERROR token to be generated somewhere in the output)
                Any test with   "bytes_literal"  as a prefix: due to the issue stated above, (this lexer prints escaped values like: '\100' as opposed to '@' in the reference lexer)
                Some tests with "line_join"      as a prefix: due to a small quirk when a doublequoted long string (three double quotes) generate a newline when displayed in racket.  This occurs as part of the lexer output formatting, and not the lexer itself.  For example, a long double quoted literal is usually formated correctly, but rarely, racket produces this:
 
                input:    """
                          abc
                          def
                          """

                output:
  
                 (LIT 
                  "\nabc\ndef")
                              
                Despite the value held in racket being correct, the formater adds a newline, and we were unable to debug why.  This has no effect on the correctness of the lexer as the string produced is correct.

        
                Some tests with "string_literal" as a prefix: 

                string_literal_eof:              Same issue as tests prefixed with "error_", error token is generated differently.
                string_literal_CR:               Same issue as above (string_literal_eof).
                string_literal_escapes:          Same issue as bytes_literal_  tests. (Escaped values not printed the same).

                   

4. Lexer Implementation notes:
	Lexers:
		PYTHONIA-OPTIMUS-LEXER:
			This is the core lexer. Handles calling into the other lexers, as well as a lot of the simple and straightforward lexing.  

		luth-lexer:
			Handles the edge case of having an indent on the first line.

		lj-lexer:
			Line join lexer. Handles the cases for both normal and raw strings containing line joins. Implcit line joins are processes. Additionally comments that contain a backslash and string literals that contain a backslash will be handled by this lexer, and utimately ignored.

		comment-lex:
			Lexer for handling comments. Comments following code will be handled appropriately through this lexer, once a new line is found the lexer will pass on to the newline-lex so that indent, dedent, and newline tokens will be handled appropriately.
			
		newline-lex:
			Multiple newlines will be consumed and only one newline token is printed due to this lexor. Spaces followed by a comment will not produce an indent/dedent token. After the newline token has been generated, indents/dedents are processed.
			
		singlequote-short-lex:
		doublequote-short-lex:			
		singlequote-long-lex:
		doublequote-long-lex:
		raw-singlequote-short-lex:
		raw-doublequote-short-lex:
		raw-singlequote-long-lex:
		raw-doublequote-long-lex:
			These lexers process strings, removing the newlines following line joins. Once a quote (single or double, long or short) is found, the lexer will run and process the string until the end of the string is found. Once finished, the string will be returned. Raw strings treat a backslash as a character and not an escape sequence. Long handles the tripple quotes.


	Helper functions:
		tab_processor:
			uses the stack to track indent levels. It was necessary to have the tab processor call PYTHONIA-OPTIMUS-LEXER, allowing us to cons in the indents and dedents without creating a sublist. 

                ilj-comment and ilj-stringliteral:
 
                        keep track of comment, or string context in lj-lexer

		ilj-*:
			implicit line join methods used for tracking how far in we are, and when to return from the lexer;
