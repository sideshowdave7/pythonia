#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define stack (list 0))

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
  [(zero? (length stack)) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER test-input))]
  [(< (peek) len) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER test-input))]
  [(> (peek) len) (pop) (if (tab_pre_processor len) (cons `(DEDENT) (PYTHONIA-OPTIMUS-LEXER test-input)) (cons `(DEDENT) (tab_processor len)))]
  [else null])
  )

(define (tab_pre_processor len)
  (eq? (peek) len)
  )

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
  (pointfloat (:: (:? intpart) (:or fraction (:: intpart "."))))
  (exponentfloat (:: (:or intpart pointfloat) exponent))
  (intpart (:+ digit))
  (fraction (:: "." (:+ digit)))
  (exponent (:: (:or "e" "E") (:or "+" "-") (:+ digit)))
  (stringliteral (:: (:? stringprefix) (:or shortstring longstring)))
  (stringprefix (:or "r" "u" "R" "U"))
  (shortstring (:or (:: "'" (:* shortstringitem) "'") (:: "\"" (:* shortstringitem) "\"")))
  (longstring (:or (:: "'''" (:* longstringitem) "'''") (:: "\"\"\"" (:* longstringitem) "\"\"\"")))
  (shortstringitem (:or shortstringchar stringescapeseq))
  (longstringitem (:or longstringchar stringescapeseq))
  (shortstringchar (:~ (:or "\\" "\n" "\"")))
  (longstringchar (char-complement "\\"))
  (stringescapeseq (:: "\\" any-char))
  (bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (bytesprefix (:or "b" "B" "br" "Br" "bR" "BR" "rb" "rB" "Rb" "RB"))
  (shortbytes (:or(:: "'" shortbytesitem "'") (:: "\"" shortbytesitem "\"")))
  (longbytes (:or(:: "'''" longbytesitem "'''") (:: "\"\"\"" longbytesitem "\"\"\"")))
  (shortbytesitem (:or shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (shortbyteschar (:~ (:or "\\" "\n" "\"")))
  (longbyteschar (char-complement "\\"))
  (bytesescapeseq (:: "\\" any-char))
  (operator (:or "+"  "-"   "*"  "**" "//" "////" "%"
                 "<<" ">>"  "&"  "|"  "^"  "~"    "<"
                 ">"  "<="  ">=" "==" "!="))
  (delimiter (:or "(" ")" "[" "]" "{" "}"
                  "," ":" "." ";" "@" "="
                  "+=" "-=" "*=" "/=" "//=" "%="
                  "&=" "|=" "^=" ">>=" "<<=" "**="))
  (imagnumber (:: (:or floatnumber intpart) (:or "j" "J")))
  (punct (:or operator delimiter))
  (allbutnewline (:~ "\n"))
  (comment (:: "#" (:* allbutnewline) "\n"))
  )

(define endtoken (string))


(define PYTHONIA-OPTIMUS-LEXER
  (lexer
   [(:: "\\" "\n")  (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(:: (:or (:* "\n") (:* comment)) "\n" (:* space))  (cons `(NEWLINE) `, (if (tab_pre_processor (string-length (string-replace lexeme "\n" "")))
                                                  (PYTHONIA-OPTIMUS-LEXER input-port)
                                                  (tab_processor (string-length (string-replace lexeme "\n" "")))))]                                                
   [(:: id_start (:* id_rest))   (if (member lexeme python-keywords) (cons `(KEYWORD ,(string->symbol lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))
                                     (cons `(ID ,lexeme) (PYTHONIA-OPTIMUS-LEXER input-port)))]
   [#\t (cons `(ERROR ,lexeme))]
   [comment (PYTHONIA-OPTIMUS-LEXER input-port)]
   [stringliteral (cons `(LIT ,(read (open-input-string lexeme))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or floatnumber bytesliteral) (cons `(LIT ,(display lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or decimalinteger bininteger) (cons `(LIT ,(read (open-input-string lexeme))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [hexinteger (cons `(LIT ,(string->symbol (let ([o (open-output-string)]) (display (replace-numid "#xc") o) (get-output-string o)))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [imagnumber (cons `(LIT ,(read (open-input-string lexeme))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [punct (cons `(PUNCT ,lexeme) (cond [(equal? lexeme "(") (implicit-lj-lexer input-port)][else (PYTHONIA-OPTIMUS-LEXER input-port)]))]
   [" " (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(eof) `((ENDMARKER))]
   ))

(define (replace-numid lexeme)
  (string-replace (string-replace lexeme "0" "#") "o" "#"))
  

(define implicit-lj-lexer
  (lexer
   ["\n" (implicit-lj-lexer input-port)]
   [" " (implicit-lj-lexer input-port)]
   [(:or "]" ")" "}") (cons `(PUNCT ,lexeme) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [punct (cons `(PUNCT ,lexeme) (implicit-lj-lexer input-port))]
   [stringliteral (cons `(LIT ,(read (open-input-string lexeme))) (implicit-lj-lexer input-port))]
   [comment (implicit-lj-lexer input-port)]
   ))
                         


(define test-input (open-input-file "test.py" #:mode 'text))