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
  (display "(NEWLINE)\n")
  (cond 
    [(zero? (length stack)) (push len) (display "(INDENT)")]
    [(< (peek) len) (push len) (display "(INDENT)")]
    [(> (peek) len) (pop) (display "(DEDENT)") (tab_processor len) ]
    
  ))


(define-tokens jrr-tolkeins (NEWLINE INDENT DEDENT ID LIT KEYWORD PUNCT ENDMARKER))

(define-lex-abbrevs
  (space " ")
  (digit (char-range "0" "9"))
  (alpha (:or (char-range #\A #\Z) (char-range #\a #\z)))
  (alphanumeric (:or digit alpha))
  )

(define PYTHONIA-OPTIMUS-LEXER
  (lexer
   ;;[#\newline (display "(NEWLINE)\n")]
   [(concatenation "\n" (:* space))  (tab_processor (string-length lexeme))]
   [(concatenation alpha (:* alphanumeric)) (display (string-append "(ID " lexeme ")" ))]
   [#\t (display "ERROR")]
   [(eof) (display "(ENDMARKER)")]))


(define test-input (open-input-string "define funct\n  \n"))