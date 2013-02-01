#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require racket/pretty)

(define implicit-lj-level 0)

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

(define (tab_processor len port)
  (cond 
  [(< (peek) len) (push len) (cons `(INDENT) (PYTHONIA-OPTIMUS-LEXER port))]
  [(> (peek) len) (pop) (if (tab_pre_processor len) (cons `(DEDENT) (PYTHONIA-OPTIMUS-LEXER port)) (cons `(DEDENT) (tab_processor len port)))]
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
  (id_start (:or alpha "_"))
  (id_rest (:or digit alpha "_"))
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
  (exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit)))
  (stringliteral (:: (:? stringprefix) (:or shortstring longstring)))
  (stringprefix (:or "r" "u" "R" "U"))
  (normalprefix (:or "u" "U" "b" "B"))
  (unicodestringprefix (:or "U" "u"))
  (shortstring (:or (:: "'" (:* shortstringsingleitem) "'") (:: "\"" (:* shortstringitem) "\"")))
  (longstring (:or (:: "'''" (:* longstringitem) "'''") (:: "\"\"\"" (:* longstringitem) "\"\"\"")))
  (shortstringitem (:or shortstringchar stringescapeseq))
  (shortstringsingleitem (:or shortstringsinglechar stringescapeseq))
  (shortrawstringsingleitem shortrawstringsinglechar)
  (longstringitem (:or longstringchar stringescapeseq))
  (shortrawstringitem shortrawstringchar)
  (longrawstringitem any-char)
  (shortstringchar (:~ (:or #\\ "\n" #\")))
  (shortrawstringchar (:~ (:or "\n" #\")))
  (shortstringsinglechar (:~ (:or #\\ "\n" #\')))
  (shortrawstringsinglechar (:~ (:or "\n" #\')))
  (longstringchar (char-complement #\\))
  (stringescapeseq (:: "\\" any-char))
  ;;(bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (rawbytesprefix (:or "br" "Br" "bR" "BR" "rb" "rB" "Rb" "RB"))
  (rawprefix (:or "r" "R" rawbytesprefix))
  (shortbytes (:or(:: "'" (:* shortbytesitem) "'") (:: "\"" (:* shortbytesitem) "\"")))
  (longbytes (:or(:: "'''" (:* longbytesitem) "'''") (:: "\"\"\"" (:* longbytesitem) "\"\"\"")))
  (shortbytesitem (:or shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (shortbyteschar (:~ (:or "\\" "\n" "\"")))
  (longbyteschar (char-complement #\\))
  (bytesescapeseq (:: "\\" any-char))
  (operator (:or "+"  "-"   "*"  "**" "/" "//" "%"
                 "<<" ">>"  "&"  "|"  "^"  "~" "<"
                 ">"  "<="  ">=" "==" "!="))
  (delimiter (:or "(" ")" "[" "]" "{" "}"
                  "," ":" "." ";" "@" "="
                  "+=" "-=" "*=" "/=" "//=" "%="
                  "&=" "|=" "^=" ">>=" "<<=" "**="))
  (imagnumber (:: (:or floatnumber intpart) (:or "j" "J")))
  (punct (:or operator delimiter))
  (newlines (:or "\n" "\f" "\r"))
  (allbutnewline (:~ newlines))
  (comment (:: "#" (:* allbutnewline) "\n"))
  )

(define ilj-level 0)

(define (inc-ilj-level)
  (set! ilj-level (+ 1 ilj-level)))

(define (dec-ilj-level)
  (set! ilj-level (- ilj-level 1)))

(define lj-lexer 
  (lexer
   ["\n" (if (equal? ilj-level 0) (string-append lexeme (lj-lexer input-port)) (string-append "" (lj-lexer input-port)))]
   [(:or "(" "[" "{") (begin (inc-ilj-level) (string-append lexeme (lj-lexer input-port)))]
   [(:or ")" "]" "}") (begin (dec-ilj-level) (string-append lexeme (lj-lexer input-port)))]
   [(:: "\\" "\n") (string-append "" (lj-lexer input-port))]
   [any-char (string-append lexeme (lj-lexer input-port))]
   [(eof) ""]
   ))


(define PYTHONIA-OPTIMUS-LEXER
  (lexer
   [(:: id_start (:* id_rest))   (if (member lexeme python-keywords) (cons `(KEYWORD ,(string->symbol lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))
                                     (cons `(ID ,(string-append "\"" lexeme "\"")) (PYTHONIA-OPTIMUS-LEXER input-port)))]
   [#\t (cons `(ERROR "Unexpected tab"))]
  
   [punct (cons `(PUNCT ,(string-append "\"" lexeme "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   ;;RAW STRINGS
   [(:: rawprefix "'''")    (cons `(LIT ,(string-append "\"" (raw-singlequote-long-lex input-port) "\""))  (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: rawprefix "\"\"\"") (cons `(LIT ,(string-append "\"" (raw-doublequote-long-lex input-port) "\""))  (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: rawprefix "'")      (cons `(LIT ,(string-append "\"" (raw-singlequote-short-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: rawprefix #\")      (cons `(LIT ,(string-append "\"" (raw-doublequote-short-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   ;;NORMAL
   [(:: (:? normalprefix) "'''")    (cons `(LIT ,(string-append "\"" (singlequote-long-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: (:? normalprefix) "\"\"\"") (cons `(LIT ,(string-append "\"" (doublequote-long-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: (:? normalprefix) "'")      (cons `(LIT ,(string-append "\"" (singlequote-short-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:: (:? normalprefix) #\")      (cons `(LIT ,(string-append "\"" (doublequote-short-lex input-port) "\"")) (PYTHONIA-OPTIMUS-LEXER input-port))]

   
   ;;[stringliteral  (cons `(LIT ,(stringify (string-literal lexeme))) (PYTHONIA-OPTIMUS-LEXER input-port))]
   ;;[bytesliteral   (cons `(LIT ,(stringify (byte-literal lexeme)))   (PYTHONIA-OPTIMUS-LEXER input-port))]
   [floatnumber    (cons `(LIT ,(float-proc lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [decimalinteger (cons `(LIT ,(string->number lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [(:or bininteger hexinteger octinteger) (cons `(LIT ,(replace-numid lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]
   [imagnumber (cons `(LIT ,(replace-imag lexeme)) (PYTHONIA-OPTIMUS-LEXER input-port))]

   [(:: (:* newlines) comment (:* space)) (cons `(NEWLINE) `, (if (tab_pre_processor (indent-length lexeme))
                                                  (PYTHONIA-OPTIMUS-LEXER input-port)
                                                  (tab_processor (indent-length lexeme) input-port)))]  
   [(:: (:* (:* newlines) (:* whitespace) (:* comment)) newlines (:* space)) (cons `(NEWLINE) `, (if (tab_pre_processor (indent-length lexeme))
                                                  (PYTHONIA-OPTIMUS-LEXER input-port)
                                                  (tab_processor (indent-length lexeme) input-port)))]                                                
   [(eof) (if (> (length stack) 1) (cons (eof-dedents) `(ENDMARKER)) `((ENDMARKER)))]

   [whitespace (PYTHONIA-OPTIMUS-LEXER input-port)]
   [comment (PYTHONIA-OPTIMUS-LEXER input-port)]

   ))

;;STRING LITERAL LEXERS
;;NORMAL (NON-RAW)
(define singlequote-short-lex
  (lexer
   ["'" ""]
   [shortstringsingleitem (string-append lexeme (singlequote-short-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))

(define (float-proc lexeme)
  (if (equal? (string-ref lexeme (- (string-length lexeme) 1)) #\.) (string-append lexeme "0") lexeme))

(define doublequote-short-lex
  (lexer
   [#\" ""]
   [#\'             (string-append "\\'"  (doublequote-short-lex input-port))]
   [shortstringitem (string-append lexeme (doublequote-short-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))


(define singlequote-long-lex
  (lexer
   ["'''" ""]
   [#\"            (string-append "\\\""      (singlequote-long-lex input-port))]     
   ["\n"           (string-append "\\n"       (singlequote-long-lex input-port))]
   [longstringitem (string-append lexeme      (singlequote-long-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))

(define doublequote-long-lex
  (lexer
   ["\"\"\"" ""]
   [#\"            (string-append "\\\""      (doublequote-long-lex input-port))]
   [#\'            (string-append "\\'"       (doublequote-long-lex input-port))]
   ["\n"           (string-append "\\n"       (doublequote-long-lex input-port))]
   [longstringitem (string-append lexeme      (doublequote-long-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))

;;RAW STRING LEXERS
(define raw-singlequote-short-lex
  (lexer
   ["'" ""]
   [shortrawstringsingleitem (string-append (string-raw lexeme) (raw-singlequote-short-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))

(define raw-doublequote-short-lex
  (lexer
   [#\" ""]
   [shortrawstringitem (string-append (string-raw lexeme) (raw-doublequote-short-lex input-port))]
   [any-char "(ERROR \"unexpected char"]
   [(eof) "(ERROR \"unexpected eof"]
   ))

(define raw-singlequote-long-lex
  (lexer
   ["'''" ""]
   ["\n"           (string-append "\\n"                  (raw-singlequote-long-lex input-port))]
   [longrawstringitem (string-append (string-raw lexeme) (raw-singlequote-long-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))

(define raw-doublequote-long-lex
  (lexer
   ["\"\"\"" ""]
   ["\n"           (string-append ""                  (raw-doublequote-long-lex input-port))]
   [longrawstringitem (string-append (string-raw lexeme) (raw-doublequote-long-lex input-port))]
   [(eof) "(ERROR \"unexpected eof"]
   ))


(define (strip-newlines lst)
  (if (equal? (car lst) '(NEWLINE)) (cdr lst) lst))


(define (decrement-lj-level)
  (set! implicit-lj-level (- implicit-lj-level 1))
  implicit-lj-level)
                         
(define (replace-numid lexeme)
  (string-replace lexeme "0" "#" #:all? #f))
  
(define (replace-imag lexeme)
  (string->number (string-append "0+" (string-replace (string-replace lexeme "j" "i") "J" "i"))))

(define (eof-dedents)
  (pop)
  (when (> 1 (length stack)) (cons '(DEDENT) (eof-dedents)) 
))
  

(define (stringliteral-removelinejoints lexeme)
   (read (open-input-string lexeme)))
  
  ;;(regexp-replace #rx"\n" (read (open-input-string lexeme)) ""))

(define (indent-length lexeme)
  (string-length (car (regexp-match #rx"\n[ ]*$" lexeme))))


(define (run-display)
  ;;(print (lj-lexer (current-input-port)))
  (for-each (lambda (arg) (pretty-display arg)) (strip-newlines (PYTHONIA-OPTIMUS-LEXER (open-input-string (lj-lexer (current-input-port)))))))

(define (run-file filename)
  (current-input-port (open-input-file filename))
  (run-display))

(define (run-string str)
  (current-input-port (open-input-string str))
  (run-display))


(define (string-raw str)
  (string-replace str "\\" "\\\\"))

(define (byte-literal str)
  (match (string-downcase (substring (byte-literalraw str) 0 1))
    ["b" (substring (byte-literalraw str) 1)]
    [_ (byte-literalraw str)]
    ))

(define (byte-literalraw str)
  (match (string-downcase (substring str 0 2))
        ["rb" (string-raw (substring str 2))]
        ["br" (string-raw (substring str 2))]
        [_ str]
    ))

;(define (stringify str)
;  (stringme (regexp-replace #rx"\"\"\"$" (regexp-replace #rx"^\"\"\"" (regexp-replace #rx"'''$" (regexp-replace #rx"^'''" str "\"") "\"") "\"") "\"")))

(run-display)

(define (run-testlj filename)
  (current-input-port (open-input-file filename))
  (display (lj-lexer (current-input-port))))
            