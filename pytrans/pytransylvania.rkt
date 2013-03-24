#lang racket

; environments are sets of variables/symbols
(define empty-env (set))

; The translator will use environments to track scope.
; When a variable is local, it is in the environment.


;; transform-program : python-program -> hir-program
(define (transform-program program)
  (match program
    [`(program . ,stmts)
     ; =>
     (define stmts* (flatten-stmts stmts))
     (define globals (local-bindings stmts*))
     `(program 
       ,@(for/list ([g globals]) `(define ,g (void)))
       ,@(map (λ (s) (transform-stmt empty-env s)) stmts*))]
    
    [else (error (format "not a program: ~s~n" program))]))

; flattens internal compound statements into the outer block:
(define (flatten-stmts stmts)
  (match stmts
    [`()
     '()]
    
    [(cons `(begin . ,more-stmts) rest)
     (append more-stmts (flatten-stmts rest))]
    
    [(cons stmt rest)
     (cons stmt rest)]))


; matches augmented assignment operators:
(define-match-expander augassign
  (syntax-rules ()
    [(_) 
     (or "+=" "-=" "*=" "/=" "%="
         "&=" "|=" "^=" "<<=" ">>=" "**=" "//=")]))

; converts an augment assignment operator to the target binop:
(define (select-augassign op)
  (match op
    ["+=" '+]
    ["-=" '-]
    ["*=" '*]
    ["/=" '/]
    ["%=" 'modulo]
    ["&=" 'bitwise-and]
    ["|=" 'bitwise-or]
    ["^=" 'bitwise-xor]
    ["<<=" '<<]
    [">>=" '>>]
    ["**=" 'expt]
    ["//=" 'quotient]))
         
; determines global bindings from a group of statements:
(define (global-bindings stmts)
  (match stmts
    ['() (set)]
    
    [(cons (or `(raise . ,_) 
               `(return . ,_)
               `(expr ,_)
               `(assert . ,_)
               `(continue)
               `(break)
               `(pass)
               `(del ,_))
           rest)
     ; =>
     (global-bindings rest)]
            
    [(cons `(def (,f . ,_) . ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(= ,lvals ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(,(augassign) ,lvals ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(nonlocal . ,vars) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(global . ,vars) rest)
     ; =>
     (set-union (global-bindings rest) (apply set vars))]
    
    [(cons `(begin . ,stmts) rest)
     ; =>
     (global-bindings (append stmts rest))]
    
    [(cons `(cond (,tests ,suites) ...) rest)
     ; =>
     (global-bindings (append (apply append (map suite->stmts suites)) rest))]
     
    [(cons `(while ,_ ,suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite) rest))]
    
    [(cons `(while ,_ ,suite ,else-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite)
                             (suite->stmts else-suite)
                             rest))]
    
    [(cons `(for ,var ,_ ,suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite) 
                              rest))]
    
    [(cons `(for ,var ,_ ,suite ,else-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite)
                              (suite->stmts else-suite)
                              rest))]
        
                             
    [(cons `(try ,body-suite ((,_ ,suites) ...) ,else-suite ,finally-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts body-suite)
                             (apply append (map suite->stmts suites))
                             (if else-suite (suite->stmts else-suite) '())
                             (if finally-suite (suite->stmts finally-suite) '())
                             rest))]
    
    [else (error (format "couldn't compute global bindings for ~s~n" (car stmts)))]))

; finds all of the bindings in a list of l-values:
(define (lvals->bindings lvals)
  (match lvals
    ['()   (set)]
    [(cons (and (? symbol?) v) rest)
     (set-add (lvals->bindings rest) v)]
    [(cons _ rest)
     (lvals->bindings rest)]))

; finds all of the local bindings in a list of statements:
(define (local-bindings stmts)
  (match stmts
    ['() (set)]
    
    [(cons (or `(raise . ,_) 
               `(return . ,_)
               `(expr ,_)
               `(assert . ,_)
               `(continue)
               `(break)
               `(pass)
               `(del ,_))
           rest)
     ; =>
     (local-bindings rest)]
            
    [(cons `(def (,f . ,_) . ,_) rest)
     ; =>
     (set-add (local-bindings rest) f)]
    
    [(cons `(= ,lvals ,_) rest)
     ; =>
     (set-union (lvals->bindings lvals) (local-bindings rest))]
    
    [(cons `(,(augassign) ,lvals ,_) rest)
     ; =>
     (set-union (lvals->bindings lvals) (local-bindings rest))]
    
    [(cons `(nonlocal . ,vars) rest)
     ; =>
     (set-subtract (local-bindings rest) (apply set vars))]
    
    [(cons `(global . ,vars) rest)
     ; =>
     (set-subtract (local-bindings rest) (apply set vars))]
    
    [(cons `(begin . ,stmts) rest)
     ; =>
     (local-bindings (append stmts rest))]
    
    [(cons `(cond (,tests ,suites) ...) rest)
     ; =>
     (local-bindings (append (apply append (map suite->stmts suites)) rest))]
     
    [(cons `(while ,_ ,suite) rest)
     ; =>
     (local-bindings (append (suite->stmts suite) rest))]
    
    [(cons `(while ,_ ,suite ,else-suite) rest)
     ; =>
     (local-bindings (append (suite->stmts suite)
                             (suite->stmts else-suite)
                             rest))]
    
    [(cons `(for ,var ,_ ,suite) rest)
     ; =>
     (local-bindings (append (list `(= (,var) ,var))
                             (suite->stmts suite) 
                             rest))]
    
    [(cons `(for ,var ,_ ,suite ,else-suite) rest)
     ; =>
     (local-bindings (append (list `(= (,var) ,var))
                             (suite->stmts suite)
                             (suite->stmts else-suite)
                             rest))]
        
                             
    [(cons `(try ,body-suite ((,_ ,suites) ...) ,else-suite ,finally-suite) rest)
     ; =>
     (local-bindings (append (suite->stmts body-suite)
                             (apply append (map suite->stmts suites))
                             (if else-suite (suite->stmts else-suite) '())
                             (if finally-suite (suite->stmts finally-suite) '())
                             rest))]
    
    [else (error (format "couldn't compute local bindings for ~s~n" (car stmts)))]))
     
; generates code to set an indexed l-value:
(define (set-index env $base index expr)
  (match index
    [`(subscript ,i)
     `(unfinished)]
    
    [`(dot ,NAME)
     `(set-field! ,$base ,NAME ,(transform-expr env expr))]
    
    [else (error (format "cannot set-index: ~s~n" index))]))


; generates code to augment an indexed l-value:
(define (set-index-augassign env $base index op expr)
  (match index
    [`(subscript ,i)
     `(unfinished)]
    
    [`(dot ,NAME)
     ; =>
     (define $b (gensym 'b))
     (define new-env (set-add env $b))
     `(let ([,$b ,$base])
        (set-field! ,$b ,NAME (,op ,(transform-expr env expr)
                                   ,(transform-expr new-env `(indexed ,$b (dot ,NAME))))))]
    
    [else (error (format "cannot set-index: ~s~n" index))]))


; generates code to delete an indexed l-value:
(define (delete-index env $base index)
  (match index
    [`(subscript ,i)
     `(unfinished)]
    
    [`(dot ,NAME)
     ; =>
     (define $b (gensym 'b))
     (define new-env (set-add env $b))
     `(let ([,$b ,$base])
        (remove-field! ,$b ,NAME))]
        
    [else (error (format "cannot delete index: ~s~n" index))]))



; generates code for a statement:
(define (transform-stmt env stmt)
  (match stmt
    [`(def (,f ,vars ...) ,suite)
     ; =>
      (define new-env (set-remove (set-union (apply set vars) env) 'current-exception))
     `(,(if (set-member? env f) 'set! 'set-global!) ,f (lambda ,vars
        (call/ec (lambda (return)
                   ,(transform-body-suite (apply set vars) new-env suite)))))]
    
    [`(= (,(and (? symbol?) var)) ,expr)
     ; =>
     (if (set-member? env var)
         `(set! ,var ,(transform-expr env expr))
         `(set-global! ,var ,(transform-expr env expr)))]
    
    [`(= ((indexed ,base ,trailers ... ,index)) ,value)
     ; =>
     `(unfinished)]
    
    [`(= ,(and lvals `(,_ ,_ . ,_)) ,expr)
     ; =>
     (define $t (gensym 't))
     (define i 0)
     `(unfinished)]
                
    [`(,(and aug-op (augassign)) (,(and (? symbol?) lval)) ,expr)
     ; =>
     (define global? (and (symbol? lval) (not (set-member? env lval))))
     (define op (select-augassign aug-op))
     ;(printf "transforming with env ~s: ~s~n" env expr)
     (define result `(,op ,(if global? `(get-global ,lval) lval) ,(transform-expr env expr)))
     `(unfinished)]
    
    
    [`(,(and aug-op (augassign)) ((indexed ,base ,trailers ... ,index)) ,value)
     ; =>
     `(unfinished)]
    
    [`(,(or '= (augassign)) ,_ ,_)
     (error "invalid assignment")]
     
    [`(del (indexed ,base ,trailers ... ,index))
     ; =>
     (delete-index
      env
      (transform-expr env `(indexed ,base ,@trailers))
      index)]
    
    [`(pass)
     ; =>
     '(void)]
    
    ['(break)
     ; =>
     '(break)]
    
    ['(continue)
     ; =>
     '(continue)]
    
    ['(return)
     ; =>
     '(return (void))]
    
    [`(return ,e)
     `(unfinished)]
    
    [`(return . ,exprs)
     `(unfinished)]
    
    [`(raise)
     '(throw current-exception)]
    
    [`(raise ,expr)
     `(unfinished)]
     
    [`(raise ,ex1 ,ex2)
      `(throw (chain-exception ,(transform-expr env ex1)
                               ,(transform-expr env ex2)))]
    
    [`(begin . ,stmts)
     ; =>
     (error (format "mis-placed begin: ~s~n" stmts))]
    
    [`(assert ,expr)
     ; =>
     `(assert ,(transform-expr env expr))]
    
    [`(assert ,expr1 ,expr2)
     ; =>
     `(unfinished)]
    
    [`(expr ,expr)
     ; =>
     (transform-expr env expr)]
    
    [`(cond [,tests ,suites] ... [else ,otherwise])
     ; =>
     `(unfinished)]
    
    [`(cond [,tests ,suites] ...)
     ; =>
     `(cond ,@(map (λ (t s) (list (transform-expr env t)
                                  (transform-suite env s))) tests suites))]
    
    [`(while ,test ,suite)
     ; =>
     `(unfinished)]
    
    [`(while ,test ,suite ,else-suite)
     ; =>
     `(unfinished)]
    
    [`(for ,var ,seq ,suite)
     ; =>
     (define $i (gensym 'i))
     `(unfinished)]
     
    [`(for ,var ,seq ,suite ,else-suite)
     ; =>
     (define $i (gensym 'i))
     `(unfinished)]
    
    [`(try ,suite (((except) ,on-except)) #f #f)
     ; =>
     `(unfinished)]
    
    [`(,(or 'global 'nonlocal) . ,_)
     ; =>
     `(void)]
    
    [else
     (error (format "no match for statement ~s~n" stmt))]))
  
; a curried form of transform-stmt, useful in conjuction with map:
(define (transform-stmt-with env)
  (λ (stmt)
    (transform-stmt env stmt)))

; selects the HIR comparison op given the Python op:
(define (select-cmp cmp)
  (match cmp
    ["<"   '<]
    [">"   '>]
    ["=="  'equal?]
    [">="  '>=]
    ["<="  '<=]
    ["!="  'not-equal?]
    
    ["in"     'in?]
    ["is"     'eq?]
    ["not-in" 'not-in?]
    ["is-not" 'not-eq?]
    
    ['in     'in?]
    ['is     'eq?]
    ['not-in 'not-in?]
    ['is-not 'not-eq?]))

; selects the HIR shift op given the Python op:
(define (select-shift op)
  (match op
    ["<<"  '<<]
    [">>"  '>>]))

; selects the HIR arithmetic op given the Python op:
(define (select-arith op)
  (match op
    ["-"  '-]
    ["+"  '+]))

; selects the HIR term op given the Python op:
(define (select-term op)
  (match op
    ["*"  '*]
    ["%"  'modulo]
    ["/"  '/]
    ["//" 'quotient]))
    
; unfolds a comparison exp in Python into an HIR exp:
(define (unwind-comparison env expr ops)
  (match ops
    ['() 
     (transform-expr env expr)]
     
    [`((,cmp ,rhs))
     `(unfinished)]
     
    [(cons (list cmp rhs) rest)
     (define $cv (gensym 'cv))
     (define new-env (set-add env $cv))
     `(unfinished)]
    
    [else (error (format "no match: ~s ~s~n" expr ops))]))

; unfolds a binary op exp in Python into an HIR exp:
(define (unwind-op select-op env $expr ops)
  (match ops
    ['() 
     (transform-expr env $expr)]
     
    [`((,op ,rhs))
     `(,(select-op op) 
       ,$expr
       ,(transform-expr env rhs))]
    
    [(cons (list op rhs) rest)
     (unwind-op select-op 
                env 
                `(,(select-op op) ,$expr
                                  ,(transform-expr env rhs))
                rest)]))

; unfolds a trailer in Python into an HIR ep:
(define (unwind-trailer env $expr trailer)
  (match trailer
    [`(dot ,NAME)
     `(get-field ,$expr ,NAME)]
    
    [`(called . ,args)
     `(unfinished)]
    
    [`(subscript ,i)
     (define $i (gensym 'i))
     (define $e (gensym 'e))
     `(let ([,$e ,$expr])
        (let ([,$i ,(transform-expr env i)])
          (cond
            [(py-list? ,$e)  (py-list-ref ,$e ,$i)]
            [(tuple? ,$e)    (tuple-ref ,$e ,$i)]
            [(dict? ,$e)     (dict-ref ,$e ,$i)]
            [else            (error "cannot index object")])))]
          
    [else (error (format "unknown trailer: ~s~n" trailer))]))
    
; unfolds a sequence of trailers into an HIR exp:
(define (unwind-trailers env $expr trailers)
  (match trailers
    ['() 
     $expr]
     
    [`(,trailer)
     (unwind-trailer env $expr trailer)]
       
    [(cons trailer rest)
     (cons trailer rest)]))
                      
     
; transforms a Python exp into an HIR exp:
(define (transform-expr env expr)
  (match expr
    ['print       'py-print]
    ['True        #t]
    ['False       #f]
    ['Ellipsis    '(quote Ellipsis)]
    ['None        '(quote None)]
    [(? number?)  expr]
    [(? string?)  expr]
    
    [(? symbol?)  
     (if (set-member? env expr)
         expr
         `(get-global ,expr))]
    
    [`(if ,cond ,true ,false)
     ; =>
     `(if ,(transform-expr env cond)
          ,(transform-expr env true)
          ,(transform-expr env false))]
    
    [`(lambda ,vars ,expr)
     ; =>
      (define new-env (set-union env (apply set vars)))
     `(unfinished)]
    
    [`(,(and op (or 'or 'and 'bitwise-or 'bitwise-and 'bitwise-xor)) 
       . ,exprs)
     ; =>
     `(,op ,@(map (λ (a) (transform-expr env a)) exprs))]
    
    [`(not ,expr)
     ; =>
     `(not ,(transform-expr env expr))]
      
    [`(comparison ,base . ,ops)
      (unwind-op select-cmp env (transform-expr env base) ops)]
    
    [`(shift ,base . ,ops)
      (unwind-op select-shift env (transform-expr env base) ops)]
    
    [`(arith ,base . ,ops)
      (unwind-op select-arith env (transform-expr env base) ops)]
         
    [`(term ,base . ,ops)
      (unwind-op select-term env (transform-expr env base) ops)]
    
    [`("+" ,expr)
     `(+ ,(transform-expr env expr))]
    
    [`("-" ,expr)
     `(- (transform-expr env expr))]
    
    [`("~" ,expr)
     `(bitwise-not (transform-expr env expr))]
    
    [`(indexed ,expr . ,trailers)
     `(,(unwind-trailers env (transform-expr env expr) trailers))]
    
    [`(power ,base ,expn)
     `(expt ,(transform-expr env base) ,(transform-expr env expn))]
    
    [`(set . ,exprs)
     `(set ,@(map (λ (a) (transform-expr env a)) exprs))]
    
    [`(tuple . ,exprs)
     `(tuple ,@(map (λ (a) (transform-expr env a)) exprs))]
    
    [`(list . ,exprs)
     `(py-list* ,@(map (λ (a) (transform-expr env a)) exprs))]
    
    [`(dict . ,pairs)
     `(dict ,@(map (λ (a) (list (transform-expr env (car a)) (transform-expr env (list-ref a 1)))) pairs))]
       
    [else         
     (error (format "cannot transform expr: ~s~n" expr))]))

; transform a suite into a list of statements:
(define (suite->stmts suite)
  (match suite
    [`(suite . ,stmts) stmts]
    [`(begin . ,stmts) stmts]
    [stmt              (list stmt)]))
   
; transform a suite that begins a new scope:
(define (transform-body-suite params env suite)
  (match suite
    [`(,(or 'suite 'begin) . ,(app flatten-stmts stmts))
     ; =>
      (define locals  (local-bindings stmts))
      (define globals (global-bindings stmts))
      (define new-env (set-subtract (set-union locals env) globals))
     `(let (,@(for/list ([v (set-subtract locals params)])
                (list v '(void))))
        ,@(map (λ (s) (transform-stmt new-env s)) stmts))]
    
    [else (transform-stmt env suite)]))

; transform a suite that does not begin a new scope:
(define (transform-suite env suite)
  (match suite
    [`(,(or 'suite 'begin) . ,(app flatten-stmts stmts))
     ; =>
     `(let ()
        ,@(map (transform-stmt-with env) stmts))]
        
    [else (transform-stmt env suite)]))


(define input #f)


(match (current-command-line-arguments)
  [(vector)
   (define in (current-input-port))
   (set! input (read in))])
  

(pretty-write (transform-program input))

   
