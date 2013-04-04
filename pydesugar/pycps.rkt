#lang racket
; primitive-operations is a set of symbols
(define primitive-operations
; primitive-operation? : symbol -> boolean
(define (primitive-operation? prim-op)
; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
; define? : term -> boolean
(define (define? sx)
; atomic? : exp -> boolean
(define (atomic? exp)
; cps-transform-program : cps-transforms a desugared program
(define (cps-transform-program program)
; cps-transform-def : cps transforms a variable definition
(define (cps-transform-def def)
; app : construct a call form
; ex: (app 'f 1 2 'x) => '(f 1 2 x)
(define (app f . args)
; cont->kont converts a syntactic continuation
;            into a meta-continuation
; ex: (cont->kont '(λ (x) (f x))
;       =>
;     (λ (rv) `((λ (x) (f x)) ,rv)
(define (cont->kont q)
; kont->cont converts a meta-continuation
;            into a syntactic continuation
; ex: (kont->cont k) 
;       =>
;    `(λ ($rv123) ,(k $rv123))
(define (kont->cont k)
; let-cont fakes a let form by immediately
;          applying a lambda form
(define (let-cont v exp body)
; let-name binds an expression to a value if
;          duplicatin it code bloat code
(define (let-name exp k)
; cps-atom converts an atomic value to an atomic cps value
(define (cps-atom exp)
; cps-transform-k converts an expression to cps and calls
;                 k with an atom holding its return value;
;                 k is a meta-continuation
(define (cps-transform-k exp k)
; cps-transform-q converts an expression to cps and
;                 inserts a call to q with its return value
;                 q is a syntactic continuation
(define (cps-transform-q exp q)
; cps-transform-k* : exp list (exp list -> answer) -> answer
(define (cps-transform-k* exps k)
(pretty-write (cps-transform-program (read)))
