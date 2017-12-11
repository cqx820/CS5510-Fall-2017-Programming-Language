#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (arg : (listof symbol))
         (body : ExprC)
         (env : Env)]
  [boolV (b : boolean)]
  [pairV (f : Value)
         (s : Value)])
;;  [pairV (f : Thunk)
;;       (r : Thunk)])

;;(define-type Thunk
;;  [delay (body : ExprC)
;;         (env : Env)
;;         (done : (boxof (optionof Value)))])


(define-type ExprC
  [numC (n : number)]
  [trueC]
  [falseC]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : (listof symbol))
        (arg-type : (listof Type))
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : (listof ExprC))]
  [eqlC (l : ExprC)
        (r : ExprC)]
  [ifC (tst : ExprC)
       (thn : ExprC)
       (els : ExprC)]
  [pairC (fst : ExprC)
         (snd : ExprC)]
  [fstC (a : ExprC)]
  [rstC (a : ExprC)])
;;[pairC (l : ExprC)
;;      (r : ExprC)]
;;[fstC (a : ExprC)]
;;[sndC (a : ExprC)])

(define-type Type
  [numT]
  [boolT]
  ;;[arroT]
  [arrowT (arg : (listof Type))
          (result : Type)]
  [starT (l : Type)
         (r : Type)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define-type TypeBinding
  [tbind (name : symbol)
         (type : Type)])

(define-type-alias TypeEnv (listof TypeBinding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true)  
  (test (interp (parse '{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
        (numV 5))
  (test/exn (interp (parse '{if {+ 3 1} 1 2})
                    mt-env)
            "not a boolean")
  (test/exn (typecheck (parse '{if {+ 3 1} 1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{= false 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{= 2 true})
                       mt-env)
            "no type")

  (test/exn (typecheck (parse '{if true 1 false})
                       mt-env)
            "no type")
  
  (test (typecheck (parse '{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{+ 1 {if true true false}})
                       mt-env)
            "no type")
  (test (interp (parse '{pair 10 8})
                mt-env)
        ;; Your constructor might be different than pairV:
        (pairV (numV 10) (numV 8)))
  
  (test (interp (parse '{fst {pair 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse '{rst {pair 10 8}})
                mt-env)
        (numV 8))
  
  (test/exn (interp (parse '{fst 10}) mt-env)
            "not a pair")
  
  (test/exn (interp (parse '{rst 10}) mt-env)
            "not a pair")
  
  (test/exn (typecheck (parse '{fst 10}) mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{rst 10}) mt-env)
            "no type")

  (test (typecheck (parse '{pair 10 8})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (starT (numT) (numT)))
  
  (test (typecheck (parse '{fst {pair 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{+ 1 {rst {pair 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{lambda {[x : (num * bool)]}
                             {fst x}})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (arrowT (list (starT (numT) (boolT))) (numT)))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {fst x}}
                            {pair 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {rst x}}
                            {pair 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{fst 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{+ 1 {fst {pair false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{lambda {[x : (num * bool)]}
                                 {if {fst x}
                                     1
                                     2}})
                       mt-env)
            "no type")
  (test (interp (parse '{{lambda {}
                           10}})
                mt-env)
        (numV 10))
  
  (test (interp (parse '{{lambda {[x : num] [y : num]} {+ x y}}
                         10
                         20})
                mt-env)
        (numV 30))
  (test (interp (parse '{{lambda {[x : num] [y : num] [z : num] [a : num]} {* {+ {+ x z} y} a}}
                         2
                         3 4 5})
                mt-env)
        (numV 45))
  (test/exn (interp (parse '{{lambda {[x : num] [y : num] [z : num]} {+ {x z} y}}
                             10
                             20})
                    mt-env)
            "wrong arity")
  (test/exn (interp (parse '{{lambda {[x : num] [y : num] [z : num]} {x z y}}
                             10
                             20
                             30})
                    mt-env)
            "not a function")
  
  (test (typecheck (parse '{{lambda {[x : num] [y : bool]} y}
                            10
                            false})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{{lambda {[x : num] [y : bool]} y}
                                false
                                10})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{{lambda {[x : num] [y : bool] [z : bool] [a : num]} y}
                                15
                                false
                                false})
                       mt-env)
            "wrong arity")
  (test/exn (typecheck (parse '{{lambda {[x : num] [y : bool] [z : bool] [a : num]} y}
                                15
                                false
                                false
                                true})
                       mt-env)
            "no type"))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `true s) (trueC)]
    [(s-exp-match? `false s) (falseC)]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{= ANY ANY} s)
     (eqlC (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (list (s-exp->symbol (first bs)))
                   (list (parse-type (third bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]
    
    [(s-exp-match? '{lambda {[SYMBOL : ANY] ...} ANY} s)
     (let* (
            ;;[arg (s-exp->list
            ;;     (first (s-exp->list 
            ;;             (second (s-exp->list s)))))]
            [args (map (lambda (x) (s-exp->symbol (first (s-exp->list x))))
                       (s-exp->list (second (s-exp->list s))))]
            [types (map (lambda (x) (parse-type (third (s-exp->list x))))
                        (s-exp->list (second (s-exp->list s))))])
       (lamC args
             types
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{if ANY ANY ANY} s)
     (ifC (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{pair ANY ANY} s)
     (pairC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{fst ANY} s)
     (fstC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{rst ANY} s)
     (rstC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY ...} s)
     (appC (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : s-expression]) : Type
  (cond
    [(s-exp-match? `num s) 
     (numT)]
    [(s-exp-match? `bool s)
     (boolT)]
    ;; [(s-exp-match? `-> s)
    ;;  [arroT]]
    [(s-exp-match? `(ANY ... -> ANY) s)
     (local [(define last-index (length (s-exp->list s)))]
       (begin
         (set! last-index (- last-index 1))
         (arrowT (map parse-type (reverse (rest (rest (reverse (s-exp->list s))))))
                 ;;(cons (list (parse-type (first (s-exp->list s)))) (map parse-type (rest (s-exp->list s))))
                 (parse-type (list-ref (s-exp->list s) last-index)))))]
    [(s-exp-match? `(ANY * ANY) s)
     (starT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test/exn (parse '{}) "invalid input")
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
  (test (parse '{let {[x : num {+ 1 2}]}
                  y})
        (appC (lamC (list 'x) (list (numT)) (idC 'y))
              (list (plusC (numC 1) (numC 2)))))
  (test (parse '{lambda {[x : num]} 9})
        (lamC (list 'x) (list (numT)) (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (list (numC 9))))
  (test (parse '{{+ 1 2}})
        (appC (plusC (numC 1) (numC 2)) empty))

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse-type '1)
            "invalid input"))


;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [trueC () (boolV true)]
    [falseC () (boolV false)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (n t body)
          (closV n body env)]
    ;;[appC (fun arg) (type-case Value (interp fun env)
    ;;                  [closV (n body c-env)
    ;;                         (interp body
    ;;                                 (extend-env
    ;;                                  (bind n
    ;;                                        (interp arg env))
    ;;                                  c-env))]
    ;;                  [else (error 'interp "not a function")])]
    [appC (fun args)
          (type-case Value (interp fun env)
            [closV (n body c-env)
                   (if (= (length n) (length args))
                       (interp body
                               (foldr extend-env c-env (map2 bind n (reverse (args-interp args empty env)))))
                       (error 'interp "wrong arity"))]
            [else (error 'interp "not a function")])]
    [eqlC (l r) (num= (interp l env) (interp r env))]
    [ifC (tst thn els)
         (type-case Value (interp tst env)
           [boolV (b) (interp (if b
                                  thn
                                  els)
                              env)]
           [else (error 'interp "not a boolean")])]
    [pairC (l r) (pairV (interp l env) (interp r env))]
    [fstC (a)
          (type-case Value (interp a env)
            [pairV (l r) l]
            [else (error 'interp "not a pair")])]
    [rstC (a)
          (type-case Value (interp a env)
            [pairV (l r) r]
            [else (error 'interp "not a pair")])]))
;;    [pairC (l r) (pairV (delay l env (box (none)))
;;                        (delay r env (box (none))))]
;;    [fstC (a) (type-case Value (interp a env)
;;                [pairV (f r) (force f)]
;;                [else (error 'interp "not a cons")])]
;;    [sndC (a) (type-case Value (interp a env)
;;                [pairV (f r) (force r)]
;;               [else (error 'interp "not a cons")])]))

;;(define (args-interp [args : (listof ExprC)] [values : (listof Value)][env : Env]) : (listof Value)
;;  (if (empty? (rest args))
;;      (cons (interp (first args) env) empty)
;;      (args-interp (rest args) (cons (interp (first args) env) values) env)))

(define (args-interp [args : (listof ExprC)] [values : (listof Value)][env : Env]) : (listof Value)
  (if (empty? args)
      values
      (args-interp (rest args) (cons (interp (first args) env) values) env)))

(module+ test
  (test (interp (parse `true) mt-env) (boolV #t))
  (test (interp (parse `false) mt-env) (boolV #f))
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse '{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))


;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> 'a)] [l : Value] [r : Value]) : 'a
  (cond
    [(and (numV? l) (numV? r))
     (op (numV-n l) (numV-n r))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (numV (num-op + l r)))
(define (num* [l : Value] [r : Value]) : Value
  (numV (num-op * l r)))
(define (num= [l : Value] [r : Value]) : Value
  (boolV (num-op = l r)))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6))
  (test (num= (numV 2) (numV 3))
        (boolV false)))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'b
    (cond
      [(empty? vals)
       (error 'find "free variable")]
      [else (if (equal? name (name-of (first vals)))
                (val-of (first vals))
                ((make-lookup name-of val-of) name (rest vals)))])))

(define lookup
  (make-lookup bind-name bind-val))

;;(define (force [t : Thunk]) : Value
;;  (type-case Thunk t
;;    [delay (b e d) (type-case (optionof Value) (unbox d)
;;                     [none ()
;;                           (let ([v (interp b e)])
;;                             (begin
;;                               (set-box! d (some v))
;;                               v))]
;;                     [some (v) v])]))


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

;; typecheck ----------------------------------------
(define (typecheck [a : ExprC] [tenv : TypeEnv])
  (type-case ExprC a
    [numC (n) (numT)]
    [trueC () (boolT)]
    [falseC () (boolT)]
    [plusC (l r) (typecheck-nums l r tenv)]
    [multC (l r) (typecheck-nums l r tenv)]
    [idC (n) (type-lookup n tenv)]
    [lamC (n arg-types body)
          (arrowT arg-types
                  (typecheck body 
                             (foldr extend-env tenv (map2 tbind n arg-types))))]
    ;;[appC (fun arg)
    ;;      (type-case Type (typecheck fun tenv)
    ;;        [arrowT (arg-type result-type)
    ;;                (if (equal? arg-type
    ;;                            (typecheck arg tenv))
    ;;                    result-type
    ;;                    (type-error arg
    ;;                               (to-string arg-type)))]
    ;;        [else (type-error fun "function")])]
    [appC (fun args)
          (type-case Type (typecheck fun tenv)
            [arrowT (arg-types result-type)
                    (if (equal? (length args) (length arg-types))
                        (if (typecheck-helper args arg-types tenv)
                            result-type
                            (type-error (first args)
                                        (to-string arg-types)))
                        (type-error fun "wrong arity"))]
            [else (type-error fun "function")])]
    [eqlC (l r)
          (type-case Type (typecheck l tenv)
            [numT ()
                  (type-case Type (typecheck r tenv)
                    [numT () (boolT)]
                    [else (type-error r "num")])]
            [else (type-error l "num")])]
    [ifC (tst thn els)
         (local [(define t-type (typecheck thn tenv))
                 (define e-type (typecheck els tenv))] 
           (type-case Type (typecheck tst tenv)
             [boolT ()
                    (cond [(equal? t-type e-type) t-type]
                          [else (type-error thn "no type")])]
             [else (type-error tst "bool")]))]
    [pairC (l r)
           (local [(define s-l (typecheck l tenv))
                   (define s-r (typecheck r tenv))]
             (starT s-l s-r))]
    [fstC (a)
          (type-case Type (typecheck a tenv)
            [starT (l r) l]
            [else (type-error a "star")])]
    [rstC (a)
          (type-case Type (typecheck a tenv)
            [starT (l r) r]
            [else (type-error a "star")])]))

(define (typecheck-helper [args : (listof ExprC)] [arg-types : (listof Type)] [tenv : TypeEnv]) : boolean
  (if (empty? args)
      true
      (if (equal? (first arg-types) (typecheck (first args) tenv))
          (typecheck-helper (rest args) (rest arg-types) tenv)
          false)))

;;(define (typecheck-helper [args : (listof ExprC)] [arg-types : (listof Type)] [tenv : TypeEnv]) : boolean
;;  (and true (map2 equal? arg-types (map (lambda (x) (typecheck x tenv)) args))))


 
(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [numT ()
          (type-case Type (typecheck r tenv)
            [numT () (numT)]
            [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test (typecheck (parse `true) mt-env) (boolT))
  (test (typecheck (parse `false) mt-env) (boolT))
  (test (typecheck (parse '10) mt-env)
        (numT))
  (test (typecheck (parse '{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse '{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

  (test (typecheck (parse '{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse '{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse '{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))