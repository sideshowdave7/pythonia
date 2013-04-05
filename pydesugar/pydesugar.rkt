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
    [`(,(or 'lambda 'λ) . ,_)     #t]
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
    [(? symbol?)      exp]
    [`(quote ,_)      (error "quotes not allowed in hir")]

    [`(letrec ((,vs ,es) ...) . ,body)
     (desugar-exp
      `(let ,(for/list ([v vs])
               (list v '(void)))
         ,@(map (λ (v e)
                  `(set! ,v ,e))
                vs es)
         ,@body))]
    
    
    [`(let ((,vs ,es) ...) . ,body)
     `((λ ,vs ,(desugar-exp `(begin ,@body))) 
       ,@(map desugar-exp es))]
        
    [`(let* () ,body)
    (desugar-body body)]
    
    [`(let* ((,v ,e) . ,rest) ,body)
     `(let ([,v ,e]) ,(desugar-exp `(let* ,rest ,body)))]
    
    [`(,(or 'lambda 'λ) ,params ,body)
     `(λ ,params ,(desugar-exp body))]

    [`(call/ec ,exp)
     `(call/ec ,(desugar-exp exp))]
    
    [`(cond)     '(void)]
    
    [`(cond (else ,exp))
     (desugar-exp exp)]
    
    [`(cond (,test ,exp))
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp) 
          (void))]
     
    [`(cond (,test ,exp) ,rest ...)
     `(if ,(desugar-exp test)
          ,(desugar-exp exp)
          ,(desugar-exp `(cond . ,rest)))]
     
    [`(and)   #t]
    [`(or)    #f]
    
    [`(or ,exp)
     (desugar-exp exp)]
    
    [`(and ,exp)
     (desugar-exp exp)]
    
    [`(or ,exp . ,rest)
     (define $t (gensym 't))
     (desugar-exp 
      `(let ((,$t ,exp))
         (if ,$t ,$t (or . ,rest))))]
     
    [`(and ,exp . ,rest)
     `(if ,(desugar-exp exp)
          ,(desugar-exp `(and . ,rest))
          #f)]
     
    [`(if ,test ,exp)
     `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
    
    [`(if ,test ,exp1 ,exp2)
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp1) 
          ,(desugar-exp exp2))]
    
    [`(set! ,v ,exp)
     `(set! ,v ,(desugar-exp exp))]

    [`(assert ,test)
     `(assert1 (λ () ,(desugar-exp test)))]
    
    [`(assert ,test ,kind)
     `(assert2 (λ () ,(desugar-exp test)) (λ () ,(desugar-exp kind)))]
    
    [`(get-global ,var)
     (global-name var)]
    
    [`(set-global! ,var ,exp)
     `(set! ,(global-name var) ,(desugar-exp exp))]
    
    [`(begin . ,exps)
     `(begin ,@(map desugar-exp exps))]
    
    ['(return)
     `(return ,exp)]
    
    ['(break)
     `(break (void))]
    
    ['(continue)
     `(continue (void))]

    [`(while ,cond ,body)
     `(call/ec ((lambda (break) ((lambda (loop) (begin (set! loop (lambda () (if ,(desugar-exp cond) (begin (call/ec (lambda(continue)  ,(desugar-exp body))) 
                                                                                                                            (loop)) (void)))) (loop) (void))) (void)))))]
                                                                         
    [`(while ,cond ,body ,else)
      `(call/ec ((lambda (break) ((lambda (loop) (begin (set! loop (lambda () (if ,(desugar-exp cond) (begin (call/ec (lambda(continue)  ,(desugar-exp body))) 
                                                                                                               (loop)) (void)))) (loop) ,(desugar-exp else))) (void)))))]
     
    [`(for-each ,var ,seq ,body ,else)
     (define $seq (gensym '$seq))
     (define $loop (gensym '$loop))
     `(call/ec 
       (lambda (break)
         ((lambda (,$seq ,$loop)
            (begin
              (begin
                (if (set? ,$seq)
                  (for-set ,$seq ,$loop)
                  (if (tuple? $seq)
                      (for-tuple ,$seq ,$loop)
                      (if (py-list? ,$seq)
                          (for-py-list ,$seq ,$loop)
                          (if (dict? ,$seq) (for-dict ,$seq ,$loop) (void)))))
              ,(desugar-exp else))))
         ,(desugar-exp seq)
         (lambda (,var)
           (call/ec
            (lambda (continue)
              ,(desugar-exp body)))))))]
    
    [`(for-each ,var ,seq ,body)
     (define $seq (gensym '$seq))
     (define $loop (gensym '$loop))
     `(call/ec 
       (lambda (break)
         ((lambda (,$seq ,$loop)
            (begin
              (begin
                (if (set? ,$seq)
                  (for-set ,$seq ,$loop)
                  (if (tuple? $seq)
                      (for-tuple ,$seq ,$loop)
                      (if (py-list? ,$seq)
                          (for-py-list ,$seq ,$loop)
                          (if (dict? ,$seq) (for-dict ,$seq ,$loop) (void)))))
              (void))))
         ,(desugar-exp seq)
         (lambda (,var)
           (call/ec
            (lambda (continue)
              ,(desugar-exp body)))))))]
         
    
    [`(dict (,keys ,values) ...)
     (define mah-list '())
     `(dict ,@(map (λ (key value) `(,key ,value)) keys values))]
    
    [`(set . ,values)
     `(set ,@values)]
    
    [`(tuple . ,values)
     `(tuple ,@(map (λ (value) (desugar-exp value)) values))]
    
    [`(py-list* . ,values)
     `(py-list* ,@values)]
    
    [`(try ,body ,handler)
     (define $ex (gensym '$ex))
     (define $ec (gensym '$ec))
   `((lambda ($old-handler)
      (begin
        ((lambda ($old-return)
           (begin
             ((lambda ($old-continue)
                (begin
                  ((lambda ($old-break)
                     (begin
                       ((lambda (return)
                          (begin
                            ((lambda (continue)
                               (begin
                                 ((lambda (break)
                                    (begin
                                      (call/ec
                                       (lambda (,$ec)
                                         (begin
                                           (set! $current-handler
                                             (lambda (,$ex)
                                               (begin
                                                 (set! $current-handler
                                                   $old-handler)
                                                 (,$ec
                                                  ((lambda (ex)
                                                     ((lambda ()
                                                        ,(desugar-exp body)))
                                                   ,$ex)))))
                                           ((lambda (rv)
                                              (begin
                                                (begin
                                                  (set! $current-handler
                                                    $old-handler)
                                                  rv)))
                                            ((lambda ()
                                              ,(desugar-exp handler)))))))))
                                  (lambda ()
                                    (begin
                                      (set! $current-handler $old-handler)
                                      ($old-break (void)))))))
                             (lambda ()
                               (begin
                                 (set! $current-handler $old-handler)
                                 ($old-continue (void)))))))
                        (lambda (rv)
                          (begin
                            (set! $current-handler $old-handler)
                            (return rv))))))
                   break)))
              continue)))
         return)))
    $current-handler))]

     
    [`(throw ,exp)
     `($current-handler ,exp)]
    
    [(? atomic?)      
     exp]

    [`(,f . ,args)  
     `(,(desugar-exp f) ,@(map desugar-exp args))]
     
    [else 
     (error (format "desugar fail: ~s~n" exp))]))

(define (desugar-body body)
  (match body
    
    [`(,exp)
     (desugar-exp exp)]
    
    [`(,(and (? not-define?) exps) ...)
     `(begin ,@(map desugar-exp exps))]
    
    [`(,tops ... ,exp)
     (define defs (tops-to-defs tops))
     (desugar-exp (match defs
                    [`((define ,vs ,es) ...)
                     `(letrec ,(map list vs es) ,exp)]))]))

(define (tops-to-defs tops)
  
  (define (top-to-def top)
    (match top
      [`(define (,f ,params ...) . ,body) 
       `(define ,f (λ ,params . ,body))]
    
      [`(define ,v ,exp)
       `(define ,v ,exp)]
    
      [exp
       `(define ,(gensym '_) ,exp)]))
  
  (map top-to-def tops))

(pretty-write (desugar-program (read (open-input-file "test.py"))))


      