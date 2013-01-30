#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require racket/pretty)

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
  [(zero? (length stack)) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER test-input))]
  [(< (peek) len) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER test-input))]
  [(> (peek) len) (pop) (if (tab_pre_processor len) (cons `(DEDENT) (PYTHONIA-OPTIMUS-LEXER test-input)) (cons `(DEDENT) (tab_processor len)))]
  ))

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
  (id_start (:or alphabetic "_"))
  (id_rest (:or digit alphabetic "_"))
  (Lu (char-range #\A #\Z))
  (Ll (char-range #\a #\z))
  (integer (:or decimalinteger octinteger hexinteger bininteger))
  (decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ "0")))
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

(define PYTHONIA-OPTIMUS-LEXER
  (lexer
   [(:: "\\" (:* space) "\n")  (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(:: (:* (:* "\n") (:* whitespace) (:* comment)) "\n" (:* space))  (cons `(NEWLINE) `, (if (tab_pre_processor (indent-length lexeme))
                                                  (PYTHONIA-OPTIMUS-LEXER input-port)
                                                  (tab_processor (indent-length lexeme))))]                                                
   [(:: id_start (:* id_rest))   (if (member lexeme python-keywords) (cons `(KEYWORD ,(string->symbol lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))
                                     (cons `(ID ,(string-append "\"" lexeme "\"")) (PYTHONIA-OPTIMUS-LEXER input-port)))]
   [#\t (cons `(ERROR ,lexeme))]
   [comment (PYTHONIA-OPTIMUS-LEXER input-port)]
   [stringliteral (cons `(LIT ,lexeme) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [floatnumber (cons `(LIT ,(display lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or decimalinteger bytesliteral) (cons `(LIT ,lexeme) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or bininteger hexinteger octinteger) (cons `(LIT ,(replace-numid lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [imagnumber (cons `(LIT ,(replace-imag lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [punct (cons `(PUNCT ,(string-append "\"" lexeme "\"")) (cond [(equal? lexeme "(") (implicit-lj-lexer input-port)]
                                                                 [else (PYTHONIA-OPTIMUS-LEXER input-port)]))]
   [" " (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(eof) `((ENDMARKER))]
   ))


(define implicit-lj-lexer
  (lexer
   ["\n" (implicit-lj-lexer input-port)]
   [(:: "\\" (:* space) "\n")  (implicit-lj-lexer input-port)]
   [" " (implicit-lj-lexer input-port)]
   [(:: id_start (:* id_rest))   (if (member lexeme python-keywords) (cons `(KEYWORD ,(string->symbol lexeme)) (implicit-lj-lexer input-port))
                                     (cons `(ID ,(string-append "\"" lexeme "\"")) (implicit-lj-lexer input-port)))]
   [(:or "]" ")" "}") (cons `(PUNCT ,(string-append "\"" lexeme "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [punct (cons `(PUNCT ,(string-append "\"" lexeme "\"")) (implicit-lj-lexer input-port))]
   [stringliteral (cons `(LIT ,lexeme) (implicit-lj-lexer input-port))]
   [comment (implicit-lj-lexer input-port)]
   [stringliteral (cons `(LIT ,lexeme) (implicit-lj-lexer input-port))]
   [floatnumber (cons `(LIT ,(display lexeme)) (implicit-lj-lexer input-port))]
   [(:or decimalinteger bytesliteral) (cons `(LIT ,lexeme) (implicit-lj-lexer input-port))]
   [(:or bininteger hexinteger octinteger) (cons `(LIT ,(replace-numid lexeme)) (implicit-lj-lexer input-port))]
   [imagnumber (cons `(LIT ,(replace-imag lexeme)) (implicit-lj-lexer input-port))]
   ))
                         
(define (replace-numid lexeme)
  (string-replace (string-replace lexeme "0" "#" #:all? #f) "o" "#" #:all? #f))
  
(define (replace-imag lexeme)
  (string-replace (string-replace lexeme "j" "i") "J" "i"))
  

(define (stringliteral-removelinejoints lexeme)
   (regexp-match #rx"\\ *\n" lexeme))

(define (indent-length lexeme)
  (string-length (car (regexp-match #rx"\n[ ]*$" lexeme))))



(define (run)
  (for-each (lambda (arg) (pretty-display arg)) (PYTHONIA-OPTIMUS-LEXER test-input)))


(define test-input (open-input-file "test.py" #:mode 'text))