#lang plai-typed
(require plai-typed/s-exp-match)

(define-type-alias Location number)

(define-type SymC
  [sym-cell (location : Location)
            (s : symbol)]) ;;Part 3 and 4, declare a new "storage" for symbols

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [boxV (l : Location)]
  [recV (ns : (listof symbol))
        (es : (listof ExprC))
        ;;(vs : (lisr))
        (ss : (listof SymC))]);;Part 3 and 4, a new record type value

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [recordC (ns : (listof symbol))
           (es : (listof ExprC))];; Part 3, record has two arguments which are all list
  [getC (rec : ExprC)
        (n : symbol)] ;;Since we have two parallel list for value-cell and symbol-cell, we want to get the record if those cell locations
                      ;;match
  
  [setC (rec : ExprC)
        (s : symbol)
        (val : ExprC)];; Mutating set for record. 
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (bx : ExprC)
           (val : ExprC)]
  [beginC (exprcS : (listof ExprC))]);; Part 2, takes a list for expressions, instead of just two expressions. 

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) 
        (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)

;;Part 1, modified override-store method, override if cell locations are match. 
;;(define override-store cons)
(define (override-store [sto : Storage] [stos : Store]) : Store
  (if (empty? stos)
      (cons sto mt-store)
      (if (equal? (cell-location sto) (cell-location (first stos)))
          (cons sto (rest stos))
          (cons (first stos) (override-store sto (rest stos))))))


(define-type Result
  [v*s (v : Value) (s : Store)])

(define-type SS ;; New typed defined
  [s*l (s : Store) (l : (listof SymC))])

(module+ test
  (print-only-errors true)
  (test (interp (parse '{let {[b {box 1}]}
                          {begin
                           {set-box! b 2}
                           {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))
  (test (interp (parse '{let {[b {box 1}]}
                          {begin
                           {set-box! b {+ 2 {unbox b}}}
                           {set-box! b {+ 3 {unbox b}}}
                           {set-box! b {+ 4 {unbox b}}}
                           {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))
  (test/exn (interp (parse '{unbox 1})
                    mt-env mt-store)
            "not a box")
  (test/exn (interp (parse '{set-box! 1 1})
                    mt-env mt-store)
            "not a box")
  (test (interp-expr (parse '{+ 1 4}))
        '5)
  (test (interp-expr (parse '{record {a 10} {b {+ 1 2}}}))
        `record)

  (test (interp-expr (parse '{get {record {a 10} {b {+ 1 0}}} b}))
        '1)

  (test/exn (interp-expr (parse '{get {record {a 10}} b}))
            "no such field")

  (test (interp-expr (parse '{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse '{get {get {record {r {record {z 0}}}} r} z}))
        '0)
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {get r x}}))
        '1)

  (test/exn (interp (parse '{get (box 1) b}) mt-env mt-store) "not a record")
  
  (test/exn (interp (parse '{set (box 1) b 2}) mt-env mt-store) "not a record")
  
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {begin
                                 {set r x 5}
                                 {get r x}}}))
        '5)

  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {d} r}]}
                                 {begin
                                   {set {get-r 0} x 6}
                                   {get {get-r 0} x}}}}))
        '6)

  (test (interp-expr (parse '{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        '5)
  (test (interp-expr(parse '{lambda {x} {+ x 1}})) `function)
  (test (interp-expr(parse '{box 4})) `box)
  (test (interp-expr(parse '{record {a 4}})) `record)
)

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
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{record {SYMBOL ANY} ...} s)
     (recordC (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? '{get ANY SYMBOL} s)
     (getC (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (setC (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{box ANY} s)
     (boxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{unbox ANY} s)
     (unboxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{set-box! ANY ANY} s)
     (setboxC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{begin ANY ANY ...} s)
     (beginC (map parse (rest (s-exp->list s))))]
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
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test (parse '{box 0})
        (boxC (numC 0)))
  (test (parse '{unbox b})
        (unboxC (idC 'b)))
  (test (parse '{set-box! b 0})
        (setboxC (idC 'b) (numC 0)))
   (test (interp (parse '{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [v*s (v-id sto-id) body]))
                                
;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (lookup s env) sto)]
    [plusC (l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num+ v-l v-r) sto-r)))]
    [multC (l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num* v-l v-r) sto-r)))]
    [letC (n rhs body)
          (with [(v-rhs sto-rhs) (interp rhs env sto)]
            (interp body
                    (extend-env
                     (bind n v-rhs)
                     env)
                    sto-rhs))]
    [lamC (n body)
          (v*s (closV n body env) sto)]
    ;;Part 3, parallelly alloc value to value cell and symbol to symbo cell. 
    [recordC (ns es)
             (type-case SS (parallel-alloc ns es env sto empty)
                       [s*l (s l) (v*s (recV ns es (reverse l)) s)])]
    
    ;;Part 3, to get a record, use pre-defined method "fetch" by passing find-symloc method to find the value from value cell
    [getC (e s)
          (with [(v-e sto-e) (interp e env sto)]
                (type-case Value v-e
                  [recV (ns es ss)
                        (v*s (fetch (find-symloc s ss) sto-e) sto-e)]
                  [else (error 'interp "not a record")]))]
    ;;Part 4, still interp old args and new args at beginning. Then use the symbol location to override the old arg with new arg. 
    [setC (e s nv)
          (with [(v-e sto-e) (interp e env sto)]
                (with [(v-nv sto-nv) (interp nv env sto-e)]
                      (type-case Value v-e
                        [recV (ns es ss) (v*s v-nv (override-store (cell (find-symloc s ss) v-nv) sto-e))]
                        [else (error 'interp "not a record")])))]
    
    [appC (fun arg)
          (with [(v-f sto-f) (interp fun env sto)]
            (with [(v-a sto-a) (interp arg env sto-f)]
              (type-case Value v-f
                [closV (n body c-env)
                       (interp body
                               (extend-env
                                (bind n v-a)
                                c-env)
                               sto-a)]
                [else (error 'interp "not a function")])))]
    
    [boxC (a)
          (with [(v sto-v) (interp a env sto)]
            (let ([l (new-loc sto-v)])
              (v*s (boxV l) 
                   (override-store (cell l v) 
                                   sto-v))))]
    [unboxC (a)
            (with [(v sto-v) (interp a env sto)]
              (type-case Value v
                [boxV (l) (v*s (fetch l sto-v) 
                               sto-v)]
                [else (error 'interp "not a box")]))]
    [setboxC (bx val)
             (with [(v-b sto-b) (interp bx env sto)]
               (with [(v-v sto-v) (interp val env sto-b)]
                 (type-case Value v-b
                   [boxV (l)
                         (v*s v-v
                              (override-store (cell l v-v)
                                              sto-v))]
                   [else (error 'interp "not a box")])))]
  ;;  [beginC (l r)
  ;;          (with [(v-l sto-l) (interp l env sto)]
  ;;            (interp r env sto-l))]
    ;;Part 2, a helper function is applied. 
    [beginC (e)
            ;;(let [r (rest e)])
            ;;(if (empty? r)
            ;;    (interp (first e) env sto)
            ;;    (with [(v-l sto-l) (interp (first e) env sto)]
            ;;          (interp (beginC r) env sto-l)
            (beginC-with-list e env sto)]))

(define (beginC-with-list [list : (listof ExprC)] [env : Env] [sto : Store]) : Result
  (if (= (length list) 1)
      (interp (first list) env sto)
      (with [(v-l sto-l) (interp (first list) env sto)]
            (beginC-with-list (rest list) env sto-l))))

(define (parallel-alloc [ns : (listof symbol)] [ls : (listof ExprC)] [env : Env] [sto : Store] [cs : (listof SymC)]) : SS
  (if (empty? ns)
      (s*l sto cs)
      (with [(v-r sto-r) (interp (first ls) env sto)]
            (let ([new-l (new-loc sto-r)])
              (parallel-alloc (rest ns) (rest ls) env (override-store (cell new-l v-r) sto-r) (cons (sym-cell new-l (first ns)) cs))))))

(define (find-symloc [s : symbol] [ss : (listof SymC)]) : Location
  (if (empty? ss)
      (error 'find-symloc "no such field")
      (type-case SymC (first ss)
        [sym-cell (l sym) (if (equal? s sym)
                              l
                              (find-symloc s (rest ss)))])))

(define (interp-expr [e : ExprC]) : s-expression
  (with ([v-e sto-e] (interp e mt-env mt-store))
        (type-case Value v-e
          [numV (n) (number->s-exp n)]
          [closV (arg body env) `function]
          [boxV (l) `box]
          [recV (ns vs ls) `record])))


(module+ test
  (test (interp (parse '2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse '{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse '{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusC (idC 'x) (idC 'x)) mt-env)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse '{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse '{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse '{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse '{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))

  (test/exn (interp (parse '{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

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
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (cond
   [(empty? sto) 0]
   [else (max (cell-location (first sto))
              (max-address (rest sto)))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (cond
   [(empty? sto) (error 'interp "unallocated location")]
   [else (if (equal? l (cell-location (first sto)))
             (cell-val (first sto))
             (fetch l (rest sto)))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))
