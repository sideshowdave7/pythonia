#lang racket

(define (NEWLINE)
  (printf "NEWLINE\n"))

(define (INDENT)
  (printf "INDENT\n"))

(define (DEDENT)
  (printf "DEDENT\n"))

(define (ID name)
  (printf "ID ~v\n" name))

(define (LIT value)
  (printf "LIT ~v\n" value))

(define-syntax KEYWORD
  (syntax-rules ()
      ((_ symbol)
       (printf "KEYWORD ~a\n" 'symbol))))

(define (PUNCT text)
  (printf "PUNCT ~v\n" text))

(define (ENDMARKER)
  (printf "ENDMARKER"))

(define (ERROR explanation)
  (printf "ERROR\n"))


