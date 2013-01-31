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
  [(zero? (length stack)) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER (current-input-port)))]
  [(< (peek) len) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER (current-input-port)))]
  [(> (peek) len) (pop) (if (tab_pre_processor len) (cons `(DEDENT) (PYTHONIA-OPTIMUS-LEXER (current-input-port))) (cons `(DEDENT) (tab_processor len)))]
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
  (shortstringchar (:~ (:or #\\ "\n" #\")))
  (longstringchar (char-complement #\\))
  (stringescapeseq (:: "\\" any-char))
  (bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (bytesprefix (:or "b" "B" "br" "Br" "bR" "BR" "rb" "rB" "Rb" "RB"))
  (shortbytes (:or(:: "'" (:* shortbytesitem) "'") (:: "\"" (:* shortbytesitem) "\"")))
  (longbytes (:or(:: "'''" (:* longbytesitem) "'''") (:: "\"\"\"" (:* longbytesitem) "\"\"\"")))
  (shortbytesitem (:or shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (shortbyteschar (:~ (:or "\\" "\n" "\"")))
  (longbyteschar (char-complement #\\))
  (bytesescapeseq (:: "\\" any-char))
  (operator (:or "+"  "-"   "*"  "**" "/" "//" "%"
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
   [(:: id_start (:* id_rest))   (if (member lexeme python-keywords) (cons `(KEYWORD ,(string->symbol lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))
                                     (cons `(ID ,(string-append "\"" lexeme "\"")) (PYTHONIA-OPTIMUS-LEXER input-port)))]
   [#\t (cons `(ERROR "Unexpected tab"))]
   [comment (PYTHONIA-OPTIMUS-LEXER input-port)]
   [stringliteral  (cons `(LIT ,(stringify (string-literal lexeme))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [bytesliteral   (cons `(LIT ,(stringify (byte-literal lexeme)))   (PYTHONIA-OPTIMUS-LEXER input-port))]
   [floatnumber    (cons `(LIT ,(string->number lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [decimalinteger (cons `(LIT ,(string->number lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or bininteger hexinteger octinteger) (cons `(LIT ,(replace-numid lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [imagnumber (cons `(LIT ,(replace-imag lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [punct (cons `(PUNCT ,(string-append "\"" lexeme "\"")) (cond [(equal? lexeme "(") (implicit-lj-lexer input-port)]
                                                                 [else (PYTHONIA-OPTIMUS-LEXER input-port)]))]
   [" " (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(:: (:* (:* "\n") (:* whitespace) (:* comment)) "\n" (:* space))  (cons `(NEWLINE) `, (if (tab_pre_processor (indent-length lexeme))
                                                  (PYTHONIA-OPTIMUS-LEXER input-port)
                                                  (tab_processor (indent-length lexeme))))]                                                
   [(:: "\\" (:* space) "\n")  (PYTHONIA-OPTIMUS-LEXER input-port)]
   [(eof) (if (> (length stack) 1) (cons (eof-dedents) `(ENDMARKER)) `((ENDMARKER)))]
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
   [comment (implicit-lj-lexer input-port)]
   [stringliteral  (cons `(LIT ,(stringify (string-literal lexeme))) (implicit-lj-lexer input-port))]
   [bytesliteral   (cons `(LIT ,(stringify (byte-literal lexeme)))   (implicit-lj-lexer input-port))]
   [floatnumber    (cons `(LIT ,(string->number lexeme)) (implicit-lj-lexer input-port))]
   [decimalinteger (cons `(LIT ,(string->number lexeme)) (implicit-lj-lexer input-port))]
   [(:or bininteger hexinteger octinteger) (cons `(LIT ,(replace-numid lexeme)) (implicit-lj-lexer input-port))]
   [imagnumber (cons `(LIT ,(replace-imag lexeme)) (implicit-lj-lexer input-port))]
   ))
                         
(define (replace-numid lexeme)
  (string-replace lexeme "0" "#" #:all? #f))
  
(define (replace-imag lexeme)
  (string->number (string-append "0+" (string-replace (string-replace lexeme "j" "i") "J" "i"))))

(define (eof-dedents)
  (pop)
  (when (> 1 (length stack)) (cons '(DEDENT) (eof-dedents)) 
))
  

(define (stringliteral-removelinejoints lexeme)
   (regexp-match #rx"\\ *\n" lexeme))

(define (indent-length lexeme)
  (string-length (car (regexp-match #rx"\n[ ]*$" lexeme))))


(define (run-display)
  (for-each (lambda (arg) (pretty-display arg)) (PYTHONIA-OPTIMUS-LEXER (current-input-port))))

(define (run-file filename)
  (current-input-port (open-input-file filename))
  (run-display))

(define (run-string str)
  (current-input-port (open-input-string str))
  (run-display))

(define (sb-lit-process str)
  (stringify (byte-literal str)))


(define (stringify str)
  (string-set! str 0 #\")
  (string-set! str (- (string-length str) 1) #\")
  str)

(define (string-literal str)
  (match (string-downcase (substring (byte-literalraw str) 0 1))
    ["r" (substring (byte-literalraw str) 1)]
    [_ (byte-literalraw str)]
    ))

(define (byte-literal str)
  (match (string-downcase (substring (byte-literalraw str) 0 1))
    ["b" (substring (byte-literalraw str) 1)]
    [_ (byte-literalraw str)]
    ))

(define (byte-literalraw str)
  (match (string-downcase (substring str 0 2))
        ["rb" (string-raw (substring str 2))]
        ["br" (string-raw (substring str 2))]
        [_ (string-raw str)]
    ))
  

(define (string-raw str)
  (string-replace str "\\" "\\\\"))

(run-file "test.py")
