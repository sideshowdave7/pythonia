(KEYWORD def)
(ID "fact")
(PUNCT "(")
(ID "n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(PUNCT "(")
(ID "n")
(PUNCT "<")
(LIT 0)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "n")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "n")
(PUNCT "*")
(ID "fact")
(PUNCT "(")
(ID "n")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "print")
(PUNCT "(")
(ID "fact")
(PUNCT "(")
(LIT 5)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ENDMARKER)