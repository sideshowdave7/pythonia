#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define stack (list 1))

(define (push x) 
  (set! stack (append (list x) stack )))

(define (pop)
  (define val (car stack))
  (set! stack (cdr stack))
  val
  )

(define (peek)
  (car stack))

(define (tab_processor len)
  (cond 
    [(zero? (length stack)) (push len) (display "(INDENT)")]
    [(< (peek) len) (push len) (display "(INDENT)")]
    [(> (peek) len) (pop) (display "(DEDENT)") (tab_processor len) ]
    
  ))

(define python-keywords (list "False" "class" "finally" "is" "return" "None" "continue" 
                              "for"        "lambda"     "try"        "True"      "def"
                              "from"       "nonlocal"   "while"
                              "and"        "del"        "global"     "not"       "with"
                              "as"         "elif"       "if"         "or"         "yield"
                              "assert"     "else"       "import"     "pass"
                              "break"      "except"     "in"         "raise"))

(define python-operators (list "+"  "-"   "*"  "**" "//" "////" "%"
                               "<<" ">>"  "&"  "|"  "^"  "~"    "<"
                               ">"  "<="  ">=" "==" "!="))

(define-tokens jrr-tolkeins (NEWLINE INDENT DEDENT ID LIT KEYWORD PUNCT ENDMARKER))

(define-lex-abbrevs
  (space " ")
  (digit (char-range "0" "9"))
  (alpha (:or (char-range #\A #\Z) (char-range #\a #\z)))
  (id_start (:or alpha "_"))
  (id_rest (:or digit alpha "_"))
  (Lu (char-range #\A #\Z))
  (Ll (char-range #\a #\z))
  (integer (:or decimalinteger octinteger hexinteger bininteger))
  (decimalinteger (:& nonzerodigit (:or (:* digit) (:+ "0"))))
  (nonzerodigit (char-range "1" "9"))
  (octinteger (:: "0" (:or "o" "O") (:+ octdigit)))
  (hexinteger (:: "0" (:or "x" "X") (:+ hexdigit)))
  (bininteger (:: "0" (:or "b" "B") (:+ bindigit)))
  (octdigit (char-range "0" "7"))
  (hexdigit (:or digit (char-range "a" "f") (char-range "A" "F")))
  (bindigit (:or "0" "1"))
  (floatnumber (:or pointfloat exponentfloat))
  (pointfloat (:: intpart (:or fraction intpart)))
  (exponentfloat (:: (:or intpart pointfloat) exponent))
  (intpart (:+ digit))
  (fraction (:: "." (:+ digit)))
  (exponent (:: (:or "e" "E") (:or "+" "-") (:+ digit)))
  (stringliteral (:or shortstring longstring))
  (stringprefix (:or "r" "u" "R" "U"))
  (shortstring (:or (:: "'" (:* shortstringitem) "'") (:: "\"" shortstringitem "\"")))
  (longstring (:or (:: "'''" (:* longstringitem) "'''") (:: "\"\"\"" longstringitem "\"\"\"")))
  (shortstringitem (:or shortstringchar stringescapeseq))
  (longstringitem (:or longstringchar stringescapeseq))
  (shortstringchar (complement (:or "\\" "\n" "\"")))
  (longstringchar (complement "\\"))
  (stringescapeseq (:: "\\" any-char))
  (bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (bytesprefix (:or "b" "B" "br" "Br" "bR" "BR" "rb" "rB" "Rb" "RB"))
  (shortbytes (:or(:: "'" shortbytesitem "'") (:: "\"" shortbytesitem "\"")))
  (longbytes (:or(:: "'''" longbytesitem "'''") (:: "\"\"\"" longbytesitem "\"\"\"")))
  (shortbytesitem (:or shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (shortbyteschar (complement (:or "\\" "\n" "\"")))
  (longbyteschar (complement "\\"))
  (bytesescapeseq (:: "\\" any-char))
  (operator (:or "+"  "-"   "*"  "**" "//" "////" "%"
                 "<<" ">>"  "&"  "|"  "^"  "~"    "<"
                 ">"  "<="  ">=" "==" "!="))
  )

(define (lit_processor lexeme)
  (display "(LIT ") (display lexeme) (display ")"))

(define (id_processor lexeme)
  (if (member lexeme python-keywords) (display (string-append "(KEYWORD " lexeme ")"))
   (display (string-append "(ID \"" lexeme "\")"))))

(define (op_processor lexeme)
  (display "(OP ") (display lexeme) (display ")"))


(define (run input)
  (PYTHONIA-OPTIMUS-LEXER input))


(define PYTHONIA-OPTIMUS-LEXER
  (lexer
   [#\newline (display "(NEWLINE)\n")]
   [(:: "\n" (:* space))  (tab_processor (string-length lexeme))]
   [(:: id_start (:* id_rest)) (id_processor lexeme)]
   [#\t (display "ERROR")]
   [(:or integer floatnumber stringliteral bytesliteral) (lit_processor lexeme)]
   [(:+ operator) (op_processor lexeme)]
   [(eof) (display "(ENDMARKER)")]))


(define test-input (open-input-string "def funct_1:\n  x=='5'\n"))