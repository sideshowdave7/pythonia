Univeristy of Utah
Spring 2013 
CS 5460 (Compilers)

Project 1: Python Lexer

Authors: David Hurst
         Blake Sleight


Running from STDIN to STDOUT:

      make run


Running test suite:

      make test



Lexer Implementation notes:
	Lexers:
		PYTHONIA-OPTIMUS-LEXER:
			This is the core lexer. HAndles calling into the other lexers, as well as a lot of the simple and straightforward lexing.  

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

		ilj-*:
			implicit line join methods used for tracking how far in we are, and when to return from the lexer;
