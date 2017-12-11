;;Author:Qixiang Chao
#lang plai-typed
(require plai-typed/s-exp-match)

(print-only-errors true)
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [appC (s : symbol)
        (args : (listof ExprC))] ;;Part 3, change arg to ExprC list
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [maxC (l : ExprC)
        (r : ExprC)];;Part 1
  [unletC (s : symbol)
          (e : ExprC)]) ;;Part 2

(define-type FunDefC
  [fdC (name : symbol) 
       (arg : (listof symbol)) ;;Part 3, change arg to the list of symbol 
       (body : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : number)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{max ANY ANY} s)
     (let ([num-l (second (s-exp->list s))] [num-r (third (s-exp->list s))]) ;;Bind arguments to num-l and num-r 
     (maxC (parse num-l) (parse num-r)))] ;;Recursively parse arguments if numbers{max 1 2} or operators({max {+ 4 5} {2 3}})
    
    [(s-exp-match? '{unlet SYMBOL ANY} s)
     (let ([bs (s-exp->list s)])
       (unletC (s-exp->symbol (second bs)) (parse (third bs))))] ;;Part 2

    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    
     ;;Part 3 ie. {f x y} -> {symbol symbol ...}
    [(s-exp-match? '{SYMBOL ANY ...} s)
     (appC (s-exp->symbol (first (s-exp->list s)));;Deal with function name which is first in the list
           (map parse (rest (s-exp->list s))))]  ;;Deal with the rest of expressions in the list if exist
    ;;Using map to parse every expressions
    
    [else (error 'parse "invalid input")]))
;;{define {f x y} {+ x y}} -> list
;;         second  third
(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-match? '{define {SYMBOL ANY ...} ANY} s) ;;Function define could take multiple args, use ... here
     (fdC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))));;Convert the function name to symbol
          (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s)))));;Use map to convert the rest arguments to symbol to lookup
          (parse (third (s-exp->list s))))];;Parse body
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{double 9})
        (appC 'double (list(numC 9))))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")

  (test (parse-fundef '{define {double x} {+ x x}})
        (fdC 'double (list'x) (plusC (idC 'x) (idC 'x))))
  (test/exn (parse-fundef '{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef '{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef '{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) (lookup s env)]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [maxC (l r);;Interp ExprCs to numbers and bind them to num-l, num-r
          (let ([num-l (interp l env fds)] [num-r (interp r env fds)])
          (if (> num-l num-r);;compare 
              num-l
              num-r))] ;;Part 1
    [unletC (s e) (interp e (unbind s env) fds)] ;;Part 2, delete the first bind in binding list and interp
    
    [appC (s arg) (local [(define fd (get-fundef s fds))]
                    (if (eq? (length (fdC-arg fd)) (length arg));;{f 1 2} must match {f x y}, otherwise wrong arity
                        (interp (fdC-body fd);;{+ x y}
                                (append
                                 (map2 bind (fdC-arg fd) (map (lambda(f) (interp f env fds)) arg));;{f x y} <- {f 2 3} recursively binding every argument to every func-arg
                                empty) fds)
                         (error 'interp "wrong arity")))]
    [letC (n rhs body)
          (interp body
                  (extend-env 
                   (bind n (interp rhs env fds))
                   env)
                  fds)]))

(module+ test
  (test (interp (parse '2) mt-env empty)
        2)
  (test/exn (interp (parse `x) mt-env empty)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x 9) mt-env)
                empty)
        9)
  (test (interp (parse '{+ 2 1}) mt-env empty)
        3)
  (test (interp (parse '{* 2 1}) mt-env empty)
        2)
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                empty)
        19)
  (test (interp (parse '{double 8})
                mt-env
                (list double-def))
        16)
  (test (interp (parse '{quadruple 8})
                mt-env
                (list double-def quadruple-def))
        32)
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                empty)
        10)
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                empty)
        12)
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                empty)
        5)
  (test/exn (interp (parse '{let {[y 5]}
                              {bad 2}})
                    mt-env
                    (list (parse-fundef '{define {bad x} {+ x y}})))
            "free variable"))

;; get-fundef ----------------------------------------
(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "undefined function")]
    [(cons? fds) (if (eq? s (fdC-name (first fds)))
                     (first fds)
                     (get-fundef s (rest fds)))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : number
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

;;unlet helper
(define (unbind [n : symbol] [env : Env]) : Env
  (cond
    [(empty? env) mt-env]
    [else (cond
            [(symbol=? n (bind-name (first env))) ;;if n matches the name of the first binding in bind list, return the rest of the bind list 
             (rest env)]
             [else (extend-env (first env) (unbind n (rest env)))])])) ;;otherwise, cons the first element in bind list and recursively pass the rest bind list

;;Unbind tester
(module+ test
  (test (unbind 'd (list(bind 'x 1) (bind 'y 2) (bind 'z 3))) (list(bind 'x 1) (bind 'y 2) (bind 'z 3)))
  (test (unbind 'y (list(bind 'y 1) (bind 'y 2) (bind 'x 2))) (list(bind 'y 2) (bind 'x 2)))
  (test (unbind 'x (list(bind 'x 3) (bind 'y 2) (bind 'z 1))) (list(bind 'y 2) (bind 'z 1)))
  (test (unbind 'z (list(bind 'x 3) (bind 'y 2) (bind 'z 1))) (list(bind 'x 3) (bind 'y 2)))
  (test (unbind 'z mt-env) empty))



(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x 8) mt-env))
        8)
  (test (lookup 'x (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'x 8) mt-env)))
        9)
  (test (lookup 'y (extend-env
                    (bind 'x 9)
                    (extend-env (bind 'y 8) mt-env)))
        8))

;; Testing part 1
(module+ test
  (test (interp (parse '{max 1 2})
                mt-env
                (list))
        2)
  (test (interp (parse '{max {+ 4 5} {+ 2 3}})
                mt-env
                (list))
        9)
  (test (interp (parse '{max {* 2 3} {* 3 2}})
                mt-env
                (list))
        6)
  (test (interp (parse '{max 0 0})
                mt-env
                (list))
        0)
  (test (interp (parse '{max {+ -1 5} {+ -2 3}})
                mt-env
                (list))
        4))

;; Testing for Part 2
(module+ test
    (test/exn (interp (parse '{let {[x 1]}
                             {unlet x
                              x}})
                    mt-env
                    (list))
            "free variable")
  (test (interp (parse '{let {[x 1]}
                          {+ x {unlet x 1}}})
                mt-env
                (list))
        2)
  (test (interp (parse '{let {[x 1]}
                          {let {[x 2]}
                            {+ x {unlet x x}}}})
                mt-env
                (list))
        3)
  (test (interp (parse '{let {[x 1]}
                          {let {[x 2]}
                            {let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})
                mt-env
                (list))
        6)
  (test (interp (parse '{f 2})
                mt-env
                (list (parse-fundef '{define {f z}
                                       {let {[z 8]}
                                         {unlet z
                                           z}}})))
        2))
;;Test for part 3
(module+ test
   (test (interp (parse '{f 1 2})
                mt-env
                (list (parse-fundef '{define {f x y} {+ x y}})))
        3)
  (test (interp (parse '{+ {f} {f}})
                mt-env
                (list (parse-fundef '{define {f} 5})))
        10)
  (test/exn (interp (parse '{f 1})
                    mt-env
                    (list (parse-fundef '{define {f x y} {+ x y}})))
            "wrong arity")
  (test (interp (parse '{f 1 2})
                mt-env
                (list (parse-fundef '{define {f x y} {* x y}})))
        2))