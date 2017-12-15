#lang plai-typed

(define-type Type
  [numT]
  [objT (class-name : symbol)]
  [arrayT (type : Type)] ;;array-of-<Type> Q7 Q10
  [nullT]);;null for Q4

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
         (rhs : ExprC)]
  [multC (lhs : ExprC)
         (rhs : ExprC)]
  [argC]
  [thisC]
  [nullC] ;;Q4
  [newC (class-name : symbol)
        (args : (listof ExprC))]
  [getC (obj-expr : ExprC)
        (field-name : symbol)]
  ;;Add set for Q5
  [setC (obj-expr : ExprC)
        (field-name : symbol)
        (arg-expr : ExprC)]
  ;;Q7 - Java style object array
  [newarrayC (type : Type)
             (size : ExprC)
             (init-val : ExprC)]
  [arrayrefC (arr-of : ExprC)
             (index : ExprC)]
  [arraysetC (arr-of : ExprC)
             (index : ExprC)
             (new-val : ExprC)]
  
  [sendC (obj-expr : ExprC)
         (method-name : symbol)
         (arg-expr : ExprC)]
  [ssendC (obj-expr : ExprC)
          (class-name : symbol)
          (method-name : symbol)
          (arg-expr : ExprC)])

(define-type ClassC
  [classC (name : symbol)
          (super-name : symbol) ;;Add super-name for Q7
          (field-names : (listof symbol))
          (methods : (listof MethodC))])

(define-type MethodC
  [methodC (name : symbol)
           (body-expr : ExprC)])

(define-type Value
  [numV (n : number)]
  [objV (class-name : symbol)
        (field-values : (listof (boxof Value)))] ;;Changed for Q5 (Hint: the value for each field in an object must be placed in a box.)
  [arrayV (type : Type) ;;arrayV value for Q7
          (size : number)
          (arr-vals : (listof (boxof Value)))]
  [nullV]);;non-number value

(module+ test
  (print-only-errors true))

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

;;Changed by adding unbox for Q5
(define (get-field [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof (boxof Value))]) : Value
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
  (unbox (kons-rest ((make-find kons-first)
                     name
                     (map2 kons field-names vals)))))
;;Added for Q5 by using set-box!
(define (set-field! [name : symbol]
                    [field-names : (listof symbol)]
                    [vals : (listof (boxof Value))]
                    [new-val : Value])
  (set-box! (kons-rest ((make-find kons-first)
                        name
                        (map2 kons field-names vals)))
            new-val))
;;Basically same with is-subclass? and is-subtype? from typed-class
(define (subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassC (find-class name1 t-classes)
       [classC (name super-name fields methods)
               (subclass? super-name name2 t-classes)])]))

(define (subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (subclass? name1 name2 t-classes)]
            ;;[nullT () true]
            [else false])]
    [arrayT (type1) ;;Added for Q7
            (type-case Type t2
              [arrayT (type2)
                      (subtype? type1 type2 t-classes)]
              [else false])]
    [nullT () ;;Q4
           (type-case Type t2
             [nullT () true]
             [objT (name) true]
             [else false])]
    [else (equal? t1 t2)]))
;;----------------------------------------------------------------
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
                   (list (box (numV 0)) (box (numV 1)))) ;;Changed test for Q5
        (numV 0))
  (test (local ([define vl (list (box (numV 0)) (box (numV 1)))]) ;;Test for set-field
          (begin
            (set-field! 'a
                        (list 'a 'b)
                        vl
                        (numV 666))
            (get-field 'a
                       (list 'a 'b)
                       vl)))
        (numV 666)))

;;Helper function for Q7, arraysetC
(define (list-set! [vals : (listof (boxof Value))] [index : number] [new-val : Value]) : Value
  (if (and (< index (length vals))
           (>= index 0))
      (begin
        (set-box! (list-ref vals index) new-val)
        (numV 0))
      ((error 'interp "index out of bound"))))
;;-----------------------------------------
;;> (define mylist (build-list 7 (lambda (v) (box 2))))
;;> mylist
;;- (listof (boxof number))
;;'(#&2 #&2 #&2 #&2 #&2 #&2 #&2)
;;> (list-set! mylist 3 19)
;;- void
;;> mylist
;;- (listof (boxof number))
;;'(#&2 #&2 #&2 #&19 #&2 #&2 #&2)
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
        [nullC () (nullV)] ;;Q4 null
        [newC (class-name field-exprs)
              (local [(define c (find-class class-name classes))
                      (define vals (map box (map recur field-exprs)))] ;;We need to change to a list of box of values
                (if (= (length vals) (length (classC-field-names c)))
                    (objV class-name vals)
                    (error 'interp "wrong field count")))]
        [getC (obj-expr field-name)
              (type-case Value (recur obj-expr)
                [objV (class-name field-vals)
                      (type-case ClassC (find-class class-name classes)
                        [classC (name super-name field-names methods)
                                (get-field field-name field-names 
                                           field-vals)])]
                [nullV () (error 'interp "null reference")]
                [else (error 'interp "not an object")])]
        ;;Q5
        [setC (obj-expr field-name arg-expr)
              (local [(define obj (recur obj-expr))
                      (define new-val (recur arg-expr))] ;;recur obj-expr and arg-expr
                (type-case Value obj
                  [objV (class-name field-vals)
                        (type-case ClassC (find-class class-name classes)
                          [classC (name super-name field-names methods)
                                  (begin
                                    (set-field! field-name field-names
                                                field-vals new-val) ;;Since set-field has void return type, so we need begin to return objV
                                    new-val)])]
                  [nullV () (error 'interp "null reference")]
                  [else (error 'interp "not an object")]))]
        ;;Q7 Q10 ---------------------------------------------
        [newarrayC (type size init-val)
                   (local [(define sz (recur size))
                           (define init (recur init-val))]
                     (arrayV type (numV-n sz) (build-list (numV-n sz) (lambda (v) (box init)))))]
        ;;> (build-list 5 (lambda (v) 10))
        ;;'(10 10 10 10 10)
        
        [arrayrefC (arr-of index) ;; Can I use list-ref here? ;; Yes I can
                   (local [(define arrof (recur arr-of))
                           (define idx (recur index))]
                     (if (and (< (numV-n idx) (arrayV-size arrof))
                              (>= (numV-n idx) 0))
                         (unbox (list-ref (arrayV-arr-vals arrof) (numV-n idx)))
                         (error 'interp "index out of bound")))]
        
        [arraysetC (arr-of index new-val)
                   (local [(define arrof (recur arr-of))
                           (define idx (recur index))
                           (define new-v (recur new-val))]
                     (type-case Type (arrayV-type arrof)
                       [objT (class-name)
                             (type-case Value new-v
                               [objV (object-name field-vals)
                                     (if (subclass? object-name class-name classes)
                                         (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)
                                         (error 'interp "not a subclass"))]
                               [nullV () (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)]
                               [else (error 'interp "not an object")])]
                       [arrayT (arr-t)
                               (type-case Value new-v
                                 [arrayV (type size arr-vals)
                                         (if (subtype? type arr-t classes)
                                             (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)
                                             (error 'interp "not a subtype"))]
                                 [else (error 'interp "not a subclass")])]
                       [numT ()
                             (type-case Value new-v
                               [numV (n) (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)]
                               [else (error 'interp "not a subtype")])]
                       [nullT ()
                              (type-case Value new-v
                                [nullV () (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)]
                                [objV (object-name field-vals) (list-set! (arrayV-arr-vals arrof) (numV-n idx) new-v)]
                                [else (error 'interp "not a subtype")])]))]
        ;;Q7 Q10 ends here -----------------------------------
        
        [sendC (obj-expr method-name arg-expr)
               (local [(define obj (recur obj-expr))
                       (define arg-val (recur arg-expr))]
                 (type-case Value obj
                   [objV (class-name field-vals)
                         (call-method class-name method-name classes
                                      obj arg-val)]
                   [nullV () (error 'interp "null reference")] ;;null for Q4
                   [else (error 'interp "not an object")]))]
        [ssendC (obj-expr class-name method-name arg-expr)
                (local [(define obj (recur obj-expr))
                        (define arg-val (recur arg-expr))]
                  ;;(type-case Value obj                    
                  ;;  [objV (c-name f-vals)
                  (call-method class-name method-name classes
                               obj arg-val))]))))
;;  [nullV () (error 'interp "null reference")] ;;null for Q4
;; [else (error 'interp "not an object")]))]


(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case ClassC (find-class class-name classes)
    [classC (name class-name field-names methods)
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
  (define posn35 (newC 'posn (list (numC 3) (numC 5)))) ;;added for test
  (define posn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test

  ;;Tests for Q4
  (test/exn (interp-posn (getC (nullC) 'x)) "null reference")
  (test/exn (interp-posn (sendC (nullC) 'addDist (numC 0))) "null reference")
  (test/exn (interp-posn (setC (nullC) 'x (numC 0))) "null reference")
  ;;(test/exn (interp-posn (ssendC (nullC) 'posn 'addDist (numC 0))) "null reference")

  ;;Test for Q5
  (test (interp-posn (setC (newC 'posn (list (numC 2) (numC 7))) 'x (numC 4)))
        (numV 4))
  (test (interp-posn (setC (newC 'posn3D (list (numC 2) (numC 7) (numC 15))) 'z (numC 4)))
        (numV 4))
  (test/exn (interp-posn (setC (numC 5) 'x (numC 4)))
            "not an object")
  (test/exn (interp-posn (setC (nullC) 'x (numC 4)))
            "null reference")
  ;;Test for Q7 and Q10 -----------------------------------------------
  (test (interp (newarrayC (numT) (numC 3) (numC 1)) empty (numV -1) (numV -1))
        (arrayV (numT) 3 (list (box (numV 1)) (box (numV 1)) (box (numV 1)))))

  (test (interp (arrayrefC (newarrayC (numT) (numC 3) (numC 1)) (numC 1)) empty (numV -1) (numV -1))
        (numV 1))
  ;;(test (interp (arrayrefC
  ;;               (arraysetC
  ;;                (newarrayC (numT) (numC 3) (numC 1))
  ;;                (numC 2) (numC 17)) (numC 2))
  ;;              empty (numV -1) (numV -1))
  ;;      (numV 17))
  (test/exn (interp (arrayrefC (newarrayC (numT) (numC 3) (numC 1)) (numC 3)) empty (numV -1) (numV -1))
            "index out of bound")
  (test/exn (interp (arrayrefC (newarrayC (numT) (numC 3) (numC -1)) (numC 3)) empty (numV -1) (numV -1))
            "index out of bound")
  (test (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 5) posn27)
                                (numC 2)
                                posn35))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 5) posn27)
                                (numC 2)
                                (nullC)))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (nullT) (numC 5) (nullC))
                                (numC 2)
                                posn27))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (nullT) (numC 5) posn27)
                                (numC 2)
                                posn35))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (numT) (numC 5) (numC 1))
                                (numC 2)
                                (numC 15)))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (arrayT (objT 'posn)) (numC 5) (newarrayC (objT 'posn) (numC 5) posn27))
                                (numC 2)
                                (newarrayC (objT 'posn) (numC 5) posn27)))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (arrayT (objT 'posn)) (numC 5) (newarrayC (objT 'posn) (numC 5) posn27))
                                (numC 2)
                                (newarrayC (objT 'posn3D) (numC 5) posn27)))
        (numV 0))
  (test/exn (interp-posn (arraysetC (newarrayC (arrayT (objT 'posn3D)) (numC 5) (newarrayC (objT 'posn3D) (numC 5) posn27))
                                    (numC 2)
                                    (newarrayC (objT 'posn) (numC 5) posn27)))
            "not a subtype")
  (test/exn (interp-posn (arraysetC (newarrayC (arrayT (objT 'posn3D)) (numC 5) (newarrayC (objT 'posn3D) (numC 5) posn27))
                                    (numC 2)
                                    (numC 12)))
            "not a subclass")
  (test/exn (interp-posn (arraysetC (newarrayC (arrayT (objT 'posn3D)) (numC 5) (newarrayC (objT 'posn3D) (numC 5) posn27))
                                    (numC 2)
                                    (nullC)))
            "not a subclass")
  (test/exn (interp-posn (arraysetC (newarrayC (numT) (numC 5) (numC 1))
                                    (numC 5)
                                    (numC 15)))
            "index out of bound")
  (test/exn (interp-posn (arraysetC (newarrayC (numT) (numC 5) (numC 1))
                                    (numC -1)
                                    (numC 15)))
            "index out of bound")
  (test/exn (interp-posn (arraysetC (newarrayC (numT) (numC 5) (numC 1))
                                    (numC 1)
                                    (nullC)))
            "not a subtype")
  (test/exn (interp-posn (arraysetC (newarrayC (objT 'posn3D) (numC 5) posn531)
                                    (numC 1)
                                    posn27))
            "not a subclass")
  ;;Tests end here ----------------------------------------------------------

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
        (objV 'posn (list (box (numV 2)) (box (numV 7))))) ;;Changed for Q5

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
