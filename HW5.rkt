#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [if0C (tst : ExprC)
        (thn : ExprC)
        (els : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true)
    (test (interp (parse '{letrec {[f {lambda {n}
                                            {if0 n
                                                 0
                                                 {+ {f {+ n -1}} -1}}}]}
                          {f 10}})
                 mt-env)
          (numV -10))
  (test (interp (parse (list->s-exp (list (list->s-exp (list plus '1)) '2))) mt-env)
      (numV 3))
  (test (interp (parse (list->s-exp (list (list->s-exp (list plus '1)) '1))) mt-env)
      (numV 2))
  (test (interp (parse (list->s-exp (list (list->s-exp (list plus '0)) '0))) mt-env)
      (numV 0))
  (test (interp (parse (list->s-exp (list (list->s-exp (list plus '-1)) '2))) mt-env)
      (numV 1))
  (test (interp (parse (list->s-exp (list (list->s-exp (list times '1)) '3))) mt-env)
      (numV 3))
  (test (interp (parse (list->s-exp (list (list->s-exp (list times '0)) '2))) mt-env)
      (numV 0))
   (test (interp (parse (list->s-exp (list (list->s-exp (list times '0)) '-2))) mt-env)
      (numV 0))
)

(define mk-rec-fun
    '{lambda {body-proc}
            {let {[fX {lambda {fX}
                              {let {[f {lambda {x}
                                               {{fX fX} x}}]}
                                    {body-proc f}}}]}
                 {fX fX}}})

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
    
    [(s-exp-match? '{letrec {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (appC (parse mk-rec-fun)
                   (lamC (s-exp->symbol (first bs))
                         (parse (second bs))))))]
       
    
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (if0C (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{+ {+ 3 4} 8})
        (plusC (plusC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (appC (lamC 'x (idC 'y))
              (plusC (numC 1) (numC 2))))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{if0 1 2 3})
        (if0C (numC 1) (numC 2) (numC 3)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [lamC (n body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    [if0C (tst thn els)
          (interp (if (num-zero? (interp tst env))
                      thn
                      els)
                  env)]))

(define plus
  '{lambda {x}
     {lambda {y}
       {+ y x}}})

(define times
  '{lambda{x}
     {lambda {y}
       {letrec {[f {lambda {z}
                     {if0 z 0
                          {+ {f {+ -1 z}} y}}}]}
       {f x}}}})


(module+ test
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{+ {+ 2 3} {+ 5 8}})
                mt-env)
        (numV 18))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))
  
  (test (interp (parse '{if0 0 2 3})
                mt-env)
        (numV 2))
  (test (interp (parse '{if0 1 2 3})
                mt-env)
        (numV 3))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{if0 {lambda {x} x} 2 3})
                    mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num-zero? [v : Value]) : boolean
  (type-case Value v
    [numV (n) (zero? n)]
    [else (error 'interp "not a number")]))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num-zero? (numV 0))
        #t)
  (test (num-zero? (numV 1))
        #f))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

