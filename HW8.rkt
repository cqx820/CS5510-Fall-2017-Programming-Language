#lang plai-typed
(require plai-typed/s-exp-match)

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
         (rhs : ExprC)]
  [multC (lhs : ExprC)
         (rhs : ExprC)]
  [argC]
  [thisC]
  [newC (class-name : symbol)
        (args : (listof ExprC))]
  [getC (obj-expr : ExprC)
        (field-name : symbol)]
  [sendC (obj-expr : ExprC)
         (method-name : symbol)
         (arg-expr : ExprC)]
  [ssendC (obj-expr : ExprC)
          (class-name : symbol)
          (method-name : symbol)
          (arg-expr : ExprC)]
  [selectC (fst : ExprC)
           (snd : ExprC)]
  [instanceofC (obj-expr : ExprC)
               (class-name : symbol)])

(define-type ClassC
  [classC (name : symbol)
          (sname : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodC))])

(define-type MethodC
  [methodC (name : symbol)
           (body-expr : ExprC)])

(define-type Value
  [numV (n : number)]
  [objV (class-name : symbol)
        (field-values : (listof Value))])

(module+ test
  (print-only-errors true)
  (test (interp-prog (list)
                     '{new object})
        `object)
    (test (interp-prog (list '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select 0 {new snowball 1}} size})
        '1)
  (test (interp-prog (list '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select {+ 1 2} {new snowball 1}} size})
        '2)
  
  (test/exn (interp-prog (list '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select {new snowball 1} {new snowball 1}} size})
        "not a number")

   (test/exn (interp-prog (list '{class snowball extends object
                                   {size}
                                   {zero this}
                                   {nonzero {new snowball {+ 1 {get this size}}}}})
                     '{get {select {+ 2 1} {+ 2 1}} size})
        "not an object")
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} fish})
        '0)
  
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} fish})
        '0)
  (test (interp-prog (list '{class fish extends object
                                   {size color}}
                           '{class shark extends fish
                                   {teeth}})
                     '{instanceof {new shark 1 2 3} fish})
        '0)
   (test (interp-prog (list '{class fish extends object
                                   {size color}}
                           '{class shark extends fish
                                   {teeth}}
                           '{class s-shark extends shark
                                   {teeth}}
                           '{class ss-shark extends s-shark
                                   {teeth}})
                     '{instanceof {new ss-shark 1 2 3 4 5} fish})
        '0)
   (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new fish 1 2} object})
        '0)
    (test (interp-prog (list '{class fish extends object
                                   {size color}}
                           '{class shark extends fish
                                   {teeth}}
                            '{class dog extends object
                                   {color}})
                     '{instanceof {new dog 1} shark})
        '1)
  (test (interp-prog (list '{class fish extends object
                                   {size color}})
                     '{instanceof {new object 1 2} fish})
        '1)
  (test/exn (interp-prog (list)
                         '{instanceof {+ 1 2} object})
            "not an object"))

;; ----------------------------------------

(define (make-find [name-of : ('a -> symbol)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'a
    (cond
     [(empty? vals)
      (error 'find "not found")]
     [else (if (equal? name (name-of (first vals)))
               (first vals)
               ((make-find name-of) name (rest vals)))])))

(define find-class : (symbol (listof ClassC) -> ClassC)
  (make-find classC-name))

(define find-method : (symbol (listof MethodC) -> MethodC)
  (make-find methodC-name))

;; A non-list pair:
(define-type (Pair 'a 'b)
  [kons (first : 'a) (rest : 'b)])

(define (get-field [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof Value)])
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
  (kons-rest ((make-find kons-first)
              name
              (map2 kons field-names vals))))

(module+ test
  (test/exn (find-class 'a empty)
            "not found")
  (test (find-class 'a (list (classC 'a 'object empty empty)))
        (classC 'a 'object empty empty))
  (test (find-class 'b (list (classC 'a 'object empty empty)
                             (classC 'b 'object empty empty)))
        (classC 'b 'object empty empty))
  (test (get-field 'a 
                   (list 'a 'b)
                   (list (numV 0) (numV 1)))
        (numV 0)))

;; ----------------------------------------

(define interp : (ExprC (listof ClassC) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case ExprC a
        [numC (n) (numV n)]
        [plusC (l r) (num+ (recur l) (recur r))]
        [multC (l r) (num* (recur l) (recur r))]
        [thisC () this-val]
        [argC () arg-val]
        [newC (class-name field-exprs)
              (if (equal? class-name 'object)
                (objV class-name (map recur field-exprs))   
                (local [(define c (find-class class-name classes))                                            
                       (define vals (map recur field-exprs))]
                (if (= (length vals) (length (classC-field-names c)))
                    (objV class-name vals)
                    (error 'interp "wrong field count"))))]
        
        [selectC (fst-ex snd-ex)
                 (local [(define num (recur fst-ex)) (define obj (recur snd-ex))]
                                   (type-case Value num
                                     [numV (n) (type-case Value obj
                                                 [objV (class-name field-vals)
                                                       (if (= n 0)
                                                           (call-method class-name 'zero classes obj (numV 0))
                                                           (call-method class-name 'nonzero classes obj (numV 0)))]
                                                 [else (error 'interp "not an object")])]
                                     [else (error 'interp "not a number")]))]

        [instanceofC (obj-expr super-class-name)
                     (local [(define obj (recur obj-expr))]
                       (type-case Value obj
                         [objV (class-name field-vals)
                               (cond
                                 [(equal? super-class-name 'object) (numV 0)]
                                 [(equal? super-class-name class-name) (numV 0)]
                                 [(equal? true (find-snames? classes class-name super-class-name)) (numV 0)]
                                 [else (numV 1)])]                                                                
                         [else (error 'interp "not an object")]))]
        
        [getC (obj-expr field-name)
              (type-case Value (recur obj-expr)
                [objV (class-name field-vals)
                      (type-case ClassC (find-class class-name classes)
                        [classC (name sname field-names methods)
                                (get-field field-name field-names 
                                           field-vals)])]
                [else (error 'interp "not an object")])]
        [sendC (obj-expr method-name arg-expr)
               (local [(define obj (recur obj-expr))
                       (define arg-val (recur arg-expr))]
                 (type-case Value obj
                   [objV (class-name field-vals)
                         (call-method class-name method-name classes
                                      obj arg-val)]
                   [else (error 'interp "not an object")]))]
        [ssendC (obj-expr class-name method-name arg-expr)
                (local [(define obj (recur obj-expr))
                        (define arg-val (recur arg-expr))]
                  (call-method class-name method-name classes
                               obj arg-val))]))))

;;(define (find-snames?
;;         [classes : (listof ClassC)]
;;         [class-name : symbol]
;;         [super-class-name : symbol]) : boolean
;;  (if (empty? (rest classes))
;;      false
;;      (type-case ClassC (first classes)
;;        [classC (name sname field-names methods)
;;                (if (equal? name class-name)
;;                    true
;;                    (find-snames? (rest classes) class-name super-class-name))])))

;;(define (find-snames?
;;         [classes : (listof ClassC)]
;;         [class-name : symbol]
;;         [super-name : symbol]) : boolean
;;  (local [(define c (find-class class-name classes))]
;;  (cond  
;;    [(equal? (classC-sname c) 'object) false]
;;    [(equal? (classC-sname c) super-name) true]
;;    [else (find-snames? classes (classC-sname c) super-name)])))


(define (find-snames?
         [classes : (listof ClassC)]
         [class-name : symbol]
         [super-name : symbol]) : boolean
  (local [(define c (find classes class-name))]
  (cond  
    [(equal? c 'object) false]
    [(equal? c super-name) true]
    [else (find-snames? classes c super-name)])))

(define (find
         [classes : (listof ClassC)]
         [class-name : symbol]) : symbol
  (if (empty? (rest classes))
      (classC-sname (first classes))
       (type-case ClassC (first classes)
            [classC (name sname field-names methods)
                    (if (equal? name class-name)
                        sname
                        (find (rest classes) class-name))])))
           

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case ClassC (find-class class-name classes)
    [classC (name sname field-names methods)
            (type-case MethodC (find-method method-name methods)
              [methodC (name body-expr)
                       (interp body-expr
                               classes
                               obj
                               arg-val)])]))

(define (num-op [op : (number number -> number)]
                [op-name : symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (classC
     'posn
     'object
     (list 'x 'y)
     (list (methodC 'mdist
                    (plusC (getC (thisC) 'x) (getC (thisC) 'y)))
           (methodC 'addDist
                    (plusC (sendC (thisC) 'mdist (numC 0))
                           (sendC (argC) 'mdist (numC 0))))
           (methodC 'addX
                    (plusC (getC (thisC) 'x) (argC)))
           (methodC 'multY (multC (argC) (getC (thisC) 'y)))
           (methodC 'factory12 (newC 'posn (list (numC 1) (numC 2)))))))

  (define posn3D-class
    (classC 
     'posn3D
     'posn
     (list 'x 'y 'z)
     (list (methodC 'mdist (plusC (getC (thisC) 'z)
                                  (ssendC (thisC) 'posn 'mdist (argC))))
           (methodC 'addDist (ssendC (thisC) 'posn 'addDist (argC))))))

  (define posn27 (newC 'posn (list (numC 2) (numC 7))))
  (define posn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numC 10) 
                empty (numV -1) (numV -1))
        (numV 10))
  (test (interp (plusC (numC 10) (numC 17))
                empty (numV -1) (numV -1))
        (numV 27))
  (test (interp (multC (numC 10) (numC 7))
                empty (numV -1) (numV -1))
        (numV 70))

  (test (interp-posn (newC 'posn (list (numC 2) (numC 7))))
        (objV 'posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendC posn27 'mdist (numC 0)))
        (numV 9))
  
  (test (interp-posn (sendC posn27 'addX (numC 10)))
        (numV 12))

  (test (interp-posn (sendC (ssendC posn27 'posn 'factory12 (numC 0))
                            'multY
                            (numC 15)))
        (numV 30))

  (test (interp-posn (sendC posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusC (numC 1) posn27))
            "not a number")
  (test/exn (interp-posn (getC (numC 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendC (numC 1) 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (ssendC (numC 1) 'posn 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (newC 'posn (list (numC 0))))
            "wrong field count"))

;-------------------------------------------------------------

;;#lang plai-typed

;; Make all "class.rkt" definitions available here, where
;; the "class.rkt" file must be in the same directory
;; as this one:
;;(require "class.rkt")

(define-type ExprI
  [numI (n : number)]
  [plusI (lhs : ExprI)
         (rhs : ExprI)]
  [multI (lhs : ExprI)
         (rhs : ExprI)]
  [argI]
  [thisI]
  [newI (class-name : symbol)
        (args : (listof ExprI))]
  [selectI (fst : ExprI)
           (snd : ExprI)]
  [instanceofI (obj-expr : ExprI)
               (class-name : symbol)]
  [getI (obj-expr : ExprI)
        (field-name : symbol)]
  [sendI (obj-expr : ExprI)
         (method-name : symbol)
         (arg-expr : ExprI)]
  [superI (method-name : symbol)
          (arg-expr : ExprI)])

(define-type ClassI
  [classI (name : symbol)
          (super-name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodI))])

(define-type MethodI
  [methodI (name : symbol)
           (body-expr : ExprI)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (expr-i->c [a : ExprI] [super-name : symbol]) : ExprC
  (local [(define (recur expr)
            (expr-i->c expr super-name))]
    (type-case ExprI a
      [numI (n) (numC n)]
      [plusI (l r) (plusC (recur l) (recur r))]
      [multI (l r) (multC (recur l) (recur r))]
      [argI () (argC)]
      [thisI () (thisC)]
      [newI (class-name field-exprs)
            (newC class-name (map recur field-exprs))]
      [selectI (num obj)
               (selectC (recur num) (recur obj))]
      [instanceofI (obj-expr super-class-name)
                   (instanceofC (recur obj-expr) super-class-name)]
      [getI (expr field-name)
            (getC (recur expr) field-name)]
      [sendI (expr method-name arg-expr)
             (sendC (recur expr)
                    method-name
                    (recur arg-expr))]
      [superI (method-name arg-expr)
              (ssendC (thisC)
                      super-name
                      method-name
                      (recur arg-expr))])))

(module+ test
  (test (expr-i->c (numI 10) 'object)
        (numC 10))
  (test (expr-i->c (plusI (numI 10) (numI 2)) 'object)
        (plusC (numC 10) (numC 2)))
  (test (expr-i->c (multI (numI 10) (numI 2)) 'object)
        (multC (numC 10) (numC 2)))
  (test (expr-i->c (argI) 'object)
        (argC))
  (test (expr-i->c (thisI) 'object)
        (thisC))
  (test (expr-i->c (newI 'object (list (numI 1))) 'object)
        (newC 'object (list (numC 1))))
  (test (expr-i->c (getI (numI 1) 'x) 'object)
        (getC (numC 1) 'x))
  (test (expr-i->c (sendI (numI 1) 'mdist (numI 2)) 'object)
        (sendC (numC 1) 'mdist (numC 2)))
  (test (expr-i->c (superI 'mdist (numI 2)) 'posn)
        (ssendC (thisC) 'posn 'mdist (numC 2))))

;; ----------------------------------------

(define (method-i->c [m : MethodI] [super-name : symbol]) : MethodC
  (type-case MethodI m
    [methodI (name body-expr) 
             (methodC name 
                      (expr-i->c body-expr super-name))]))

(module+ test
  (define posn3d-mdist-i-method
    (methodI 'mdist
             (plusI (getI (thisI) 'z)
                    (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (methodC 'mdist
             (plusC (getC (thisC) 'z)
                    (ssendC (thisC) 'posn 'mdist (argC)))))

  (test (method-i->c posn3d-mdist-i-method 'posn)
        posn3d-mdist-c-method))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : ClassC
  (type-case ClassI c
    [classI (name super-name field-names methods)
            (classC
             name
             super-name
             field-names
             (map (lambda (m) (method-i->c m super-name))
                  methods))]))

(module+ test
  (define posn3d-i-class 
    (classI 'posn3d
            'posn
            (list 'z)
            (list posn3d-mdist-i-method)))
  (define posn3d-c-class-not-flat
    (classC 'posn3d
            'posn
            (list 'z)
            (list posn3d-mdist-c-method)))
  
  (test (class-i->c-not-flat posn3d-i-class)
        posn3d-c-class-not-flat))

;; ----------------------------------------

(define (flatten-class [c : ClassC] 
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassC c
    [classC (name sname field-names methods)
            (type-case ClassC (flatten-super name classes i-classes)
              [classC (super-name s-sname super-field-names super-methods)
                      (classC
                       name
                       sname
                       (add-fields super-field-names field-names)
                       (add/replace-methods super-methods methods))])]))

(define (flatten-super [name : symbol]
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassI (find-i-class name i-classes)
    [classI (name super-name field-names i-methods)
            (if (equal? super-name 'object)
                (classC 'object super-name empty empty)
                (flatten-class (find-class super-name classes)
                               classes
                               i-classes))]))

(module+ test
  (define posn-i-class 
    (classI 'posn
            'object
            (list 'x 'y)
            (list (methodI 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                  (methodI 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define addDist-c-method
    (methodC 'addDist
             (plusC (sendC (thisC) 'mdist (numC 0))
                    (sendC (argC) 'mdist (numC 0)))))
  (define posn-c-class-not-flat
    (classC 'posn
            'object
            (list 'x 'y)
            (list (methodC 'mdist
                           (plusC (getC (thisC) 'x)
                                  (getC (thisC) 'y)))
                  addDist-c-method)))
  (define posn3d-c-class
    (classC 'posn3d
            'posn
            (list 'x 'y 'z)
            (list posn3d-mdist-c-method
                  addDist-c-method)))

  (test (flatten-class posn3d-c-class-not-flat
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        posn3d-c-class))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (listof MethodC)]
                             [new-methods : (listof MethodC)])
  : (listof MethodC)
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (listof MethodC)] 
                            [new-method : MethodC])
  : (listof MethodC)
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (methodC-name (first methods))
                 (methodC-name new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (methodC 'm (numC 0))))
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0))) empty)
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))))
  (test (add/replace-methods (list (methodC 'm (numC 0))
                                   (methodC 'n (numC 2)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))
                                   (methodC 'n (numC 2))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))

  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'm (numC 1)))
        (list (methodC 'm (numC 1))))
  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'n (numC 2)))
        (list (methodC 'm (numC 0))
              (methodC 'n (numC 2)))))

;; ----------------------------------------

(define find-i-class : (symbol (listof ClassI) -> ClassI)
  (make-find classI-name))

;; ----------------------------------------

(define (interp-i [i-a : ExprI] [i-classes : (listof ClassI)]) : Value
  (local [(define a (expr-i->c i-a 'object))
          (define classes-not-flat (map class-i->c-not-flat i-classes))
          (define classes
            (map (lambda (c)
                   (flatten-class c classes-not-flat i-classes))
                 classes-not-flat))]
    (interp a classes (numV -1) (numV -1))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'posn3d (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18)))

;-------------------------------------------------------

;;#lang plai-typed
;;(require plai-typed/s-exp-match
  ;;       "class.rkt"
    ;;     "inherit.rkt")

(module+ test
  (print-only-errors true))


(define (parse-class [s : s-expression]) : ClassI
  (cond
   [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
    (classI (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (fourth (s-exp->list s)))
            (map parse-field
                 (s-exp->list (fourth (rest (s-exp->list s)))))
            (map parse-method 
                 (rest (rest (rest (rest (rest (s-exp->list s))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : s-expression]) : symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : s-expression]) : MethodI
  (cond
   [(s-exp-match? `{SYMBOL ANY} s)
    (methodI (s-exp->symbol (first (s-exp->list s)))
             (parse (second (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

(define (parse [s : s-expression]) : ExprI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? '{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? '{select ANY ANY} s)
    (selectI (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
   [(s-exp-match? '{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? '{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? '{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse '{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse '{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse '{new posn 1 2})
        (newI 'posn (list (numI 1) (numI 2))))
  (test (parse '{get 1 x})
        (getI (numI 1) 'x))
  (test (parse '{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse '{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field '{x 1})
            "invalid input")

  (test (parse-method `{m this})
        (methodI 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
            "invalid input")
  
  (test (parse-class '{class posn3D extends posn
                             {x y z}
                             {m1 arg}
                             {m2 this}})
        (classI 'posn3D 'posn
                (list 'x 'y 'z)
                (list (methodI 'm1 (argI))
                      (methodI 'm2 (thisI)))))
  (test/exn (parse-class '{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [numV (n) (number->s-exp n)]
      [objV (class-name field-vals) `object])))

(module+ test
  (test (interp-prog
         (list
          '{class empty extends object
                  {}})
         '{new empty})
        `object)

 (test (interp-prog 
        (list
         '{class posn extends object
                 {x y}
                 {mdist {+ {get this x} {get this y}}}
                 {addDist {+ {send arg mdist 0}
                             {send this mdist 0}}}}
         
         '{class posn3D extends posn
                 {z}
                 {mdist {+ {get this z} 
                           {super mdist arg}}}})
        
        '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
       '18))
