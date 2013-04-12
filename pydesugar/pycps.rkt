#lang racket

; Input language:

; <aexpr> ::= (λ (<var>*) <expr>)
;          |  <var>
;          |  #t | #f
;          |  <number>
;          |  <string>
;          |  (void)
;          |  call/ec | call/cc

; <expr> ::= <aexpr>
;         |  (begin <expr>*)
;         |  (if <expr> <expr> <expr>)
;         |  (set! <var> <expr>)
;         |  (letrec ([<var> <aexpr>]*) <expr>)
;         |  (<prim> <expr>*)
;         |  (<expr> <expr>*)

; <prim> = { + , - , / , * , = }



; Output language:

; <aexp> ::= (λ (<var>*) <cexp>)
;         |  <var>
;         |  #t | #f
;         |  <number>
;         |  <string>
;         |  (void)

; <cexp> ::= (if <aexp> <cexp> <cexp>)
;         |  (set-then! <var> <aexp> <cexp>)
;         |  (letrec ([<var> <aexp>]*) <cexp>)
;         |  ((cps <prim>) <aexp>*)
;         |  (<aexp> <aexp>*)


(define prims
  (apply set '( + - / *  < > py-print not assert1 
                  set? py-list? dict? tuple? string? integer?
                  bitwise-not dict-remove! dict-ref tuple-ref
                  py-list-remove! py-list-ref
                  bitwise-and bitwise-or bitwise-xor
                  assert2 expt quotient modulo << >> equal? >= <= not-equal?
                  in? not-in? eq? not-eq? py-list-set! dict-set! tuple-set!
                  )))

(define (aexpr? expr)
  (match expr
    [(or `(,(or 'λ 'lambda) (,_ ...) ,_)
         (? symbol?)
         (? number?)
         (? string?)
         (? boolean?)
         '(void)
         `(py-list* ,_ ...)
         `(set ,_ ...)
         `(dict ,_ ...)
         `(tuple ,_ ...))
     ; =>
     #t]
    
    [else #f]))

(define (prim? term)
  (set-member? prims term))
         

(define (T-k expr k)
  (match expr
    [ (? aexpr?)  #;=>  (k (M expr))]
    
    [`(begin ,expr)
      (T-k expr k)]
    
    [`(begin ,expr ,exprs ...)
      (T-k expr (lambda (_)
                  (T-k `(begin ,@exprs) k)))]
    
    [`(if ,exprc ,exprt ,exprf)
      ; We have to reify the cont to avoid
      ; a possible code blow-up:
      (define $rv (gensym '$rv))
      (define cont `(lambda (,$rv) ,(k $rv)))
      (T-k exprc (lambda (aexp)
           `(if ,aexp 
                ,(T-c exprt cont)
                ,(T-c exprf cont))))]
    
    [`(for-set ,seq ,loop)
     `(for-set-k ,seq ,loop ,k)]
    
    [`(for-tuple ,seq ,loop)
     `(for-tuple-k ,seq ,loop ,k)]
    
    [`(for-py-list ,seq ,loop)
     `(for-py-list-k ,seq ,loop ,k)]
    
    [`(for-dict ,seq ,loop)
     `(for-dict-k ,seq ,loop ,k)]
    
    [`(set! ,var ,expr)
      (T-k expr (lambda (aexp)
                  `(set-then! ,var ,aexp
                              ,(k '(void)))))]
    
    [`(letrec ([,vs ,as] ...) ,expr)
     `(letrec (,@(map list vs (map M as))) 
        ,(T-k expr k))]
    
 
    
    [`(,_ ,_ ...)
      ; =>
      (define $rv (gensym '$rv))
      (define cont `(lambda (,$rv) ,(k $rv)))
      (T-c expr cont)]))


(define (T-c expr c)
  (match expr
    [ (? aexpr?)  #;=>  `(,c ,(M expr))]
    
    [`(begin ,expr)      (T-c expr c)]
    
    [`(begin ,expr ,exprs ...)
      (T-k expr (lambda (_)
                  (T-c `(begin ,@exprs) c)))]
    
    [`(if ,exprc ,exprt ,exprf)
      ; We have to bind the cont to avoid
      ; a possible code blow-up:
      (define $k (gensym '$k))
      ;`((lambda (,$k)
          (T-k exprc (lambda (aexp)
                        `(if ,aexp 
                             ,(T-c exprt c)
                             ,(T-c exprf c))))]
        ;)
        ;,c)]
    
    [`(set! ,var ,expr)
      (T-k expr (lambda (aexp)
                  `(set-then! ,var ,aexp
                              (,c (void)))))]

    
    [`(letrec ([,vs ,as] ...) ,expr)
     `(letrec (,@(map list vs (map M as))) 
        ,(T-c expr c))]
    
    [`(,(and p (? prim?)) ,es ...)
      ; =>
     (T*-k es (lambda ($es)
                `((cps ,p) ,@$es ,c)))]
    
    
    [`(for-set ,seq ,loop)
     (define $k (gensym '$k))
      (T-k expr $k)]
    
    
    [`(for-tuple ,seq ,loop)
     (define $k (gensym '$k))
      (T-k expr $k)]
    
   [`(for-py-list ,seq ,loop)
     (define $k (gensym '$k))
      (T-k expr $k)]
    
    [`(for-dict ,seq ,loop)
     (define $k (gensym '$k))
      (T-k expr $k)]
    
    [`(,f ,es ...)    
      ; =>
      (T-k f (lambda ($f)
             (T*-k es (lambda ($es)
                      `(,$f ,@$es ,c)))))]))

(define (T*-k exprs k)
  (cond
    [(null? exprs)   (k '())]
    [(pair? exprs)   (T-k (car exprs) (lambda (hd)
                       (T*-k (cdr exprs) (lambda (tl)
                         (k (cons hd tl))))))]))
     
(define (M aexpr)
  (match aexpr
    [`(,(or 'lambda 'λ) (,vars ...) ,body)
      ; =>
      (define $k (gensym '$k))
     `(lambda (,@vars ,$k) 
        ,(T-c body $k))]
    
    [(or 'call/ec 'call/cc)
     '(lambda (f cc) (f (lambda (x _) (cc x)) cc))]
    
    [(or (? symbol?)     
         (? number?)
         (? string?)
         (? boolean?)
         '(void)
         `(py-list* ,_ ...)
         `(set ,_ ...)
         `(dict ,_ ...)
         `(tuple ,_ ...))     
     ; =>
     aexpr]
    
    
    [else 
     (error "Not an aexpr!")]))


;; Extensions

(define (cps f)
  (λ args
    (match args
      [`(,xs ... ,k)
       (k (apply f xs))])))


(define-syntax set-then!
  (syntax-rules ()
    [(_ var exp then)
     (begin
       (set! var exp)
       then)]))
             


(define (cps-transform-program program)
  (match program
    
    [`(program . ,stmts) `(program ,@(map cps-transform-program stmts)) ]
    [`(define ,v ,e) `(define ,v ,e)]
    [stmt  (T-c stmt '$halt)]
  ))
  

(pretty-write (cps-transform-program (read (open-input-file "test.py"))))