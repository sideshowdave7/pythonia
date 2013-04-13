#lang racket

;; Helpers.

; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition-k pred (cdr lst) (λ (in out)
        (if (pred (car lst))
            (k (cons (car lst) in) out)
            (k in (cons (car lst) out)))))))

; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [`(,(or lambda 'λ) . ,_)     #t]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    ['None         #t]
    ['Ellipsis     #t]
    [else          #f]))

; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))

; global-name : symbol -> symbol
(define (global-name name)
  (string->symbol (string-append "g$" (symbol->string name))))

; atomize-tops : top list -> top list
(define (atomize-tops tops)
  (match tops
    ['()  #;=>  '()]
    
    [(cons (and head (? atomic-define?)) tail)
     (cons head (atomize-tops tail))]
    
    [(cons `(define ,v ,exp) tail)
     `((define ,v (void))
       (set! ,v ,exp)
       ,@(atomize-tops tail))]
    
    [(cons head tail)
     (cons head (atomize-tops tail))]))


      

;; Desugaring.

; desugar-top : top -> top
(define (desugar-top top)
  (match top
    [`(define ,v ,exp)
     `(define ,(global-name v) ,(desugar-exp exp))]
    
    [exp
     (desugar-exp exp)]))

     
; desugar : program -> program
(define (desugar-program program)
  
  (define prog (match program [`(program . ,stmts) stmts]))
  
  (set! prog (atomize-tops prog))
  
  (set! prog (map desugar-top prog))
  
  (set! prog (append (list
                      '(define break (void))
                      '(define return (void))
                      '(define continue (void))
                      '(define $current-handler (void)))
                     prog))
  
  (set! prog
    (partition-k 
     atomic-define?
     prog
     (λ (atomic complex)
       (append atomic `((begin ,@complex))))))
  
  `(program ,@prog))


; desugar-exp : exp -> exp
(define (desugar-exp exp)
  (match exp
    [(? symbol?)      (error "havent't handled symbols)")]
    [`(quote ,_)      (error "quotes not allowed in hir")]

    [`(letrec ((,vs ,es) ...) . ,body)
     (error "haven't handled letrec")]
     
    [`(let ((,vs ,es) ...) . ,body)
     (error "haven't handled let")]
        
    [`(let* () ,body)
     (error "haven't handled let*")]
    
    [`(let* ((,v ,e) . ,rest) ,body)
     (error "haven't handled let*")]
    
    [`(,(or 'lambda 'λ) ,params ,body)
     (error "haven't handled lambda")]

    [`(call/ec ,exp)
     (error "haven't handled call/ec")]
    
    [`(cond)
     (error "haven't handled cond")]
    
    [`(cond (else ,exp))
     (error "haven't handled cond")]
    
    [`(cond (,test ,exp))
     (error "haven't handled cond")]
     
    [`(cond (,test ,exp) ,rest ...)
     (error "haven't handled cond")]
     
    [`(and)   #t]
    [`(or)    #f]
    
    [`(or ,exp)
     (error "haven't handled or")]
    
    [`(and ,exp)
     (error "haven't handled and")]
    
    [`(or ,exp . ,rest)
     (error "haven't handled or")]
     
    [`(and ,exp . ,rest)
     (error "haven't handled and")]
     
    [`(if ,test ,exp)
     (error "haven't handled if")]
    
    [`(if ,test ,exp1 ,exp2)
     (error "haven't handled if")]
    
    [`(set! ,v ,exp)
     (error "haven't handled set!")]

    [`(assert ,test)
     (error "haven't handled assert")]
    
    [`(assert ,test ,kind)
     (error "haven't handled assert")]
    
    [`(get-global ,var)
     (error "haven't handled get-global")]
    
    [`(set-global! ,var ,exp)
     (error "haven't handled set-global!")]
    
    [`(begin . ,exps)
     (error "haven't handled begin")]
    
    ['(return)
     (error "haven't handled return")]
    
    ['(break)
     (error "haven't handled break")]
    
    ['(continue)
     (error "haven't handled continue")]

    [`(while ,cond ,body)
     (error "haven't handled while")]
    
    [`(while ,cond ,body ,else)
     (error "haven't handled while")]
     
    [`(for-each ,var ,seq ,body ,else)
     (error "haven't handled for-each")]
    
    [`(for-each ,var ,exp ,body)
     (error "haven't handled for-each")]
    
    [`(dict (,keys ,values) ...)
     (error "haven't handled dict")]
    
    [`(set . ,values)
     (error "haven't handled set")]
    
    [`(tuple . ,values)
     (error "haven't handled tuple")]
    
    [`(py-list* . ,values)
     (error "haven't handled py-list*")]
    
    [`(try ,body ,handler)
     (error "haven't handled try")]
     
    [`(throw ,exp)
     (error "haven't handled throw")]
    
    [(? atomic?)      
     (error "haven't handled atomic expressions")]

    [`(,f . ,args)  
     (error "haven't handle application forms")]
            
    [else 
     (error (format "desugar fail: ~s~n" exp))]))




(pretty-write (desugar-program (read)))


      