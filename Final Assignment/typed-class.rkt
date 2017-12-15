#lang plai-typed

(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  [classT (name : symbol)
          (super-name : symbol)
          (fields : (listof FieldT))
          (methods : (listof MethodT))])

(define-type FieldT
  [fieldT (name : symbol)
          (type : Type)])

(define-type MethodT
  [methodT (name : symbol)
           (arg-type : Type)
           (result-type : Type)
           (body-expr : ExprI)])

;;(define-type Type
;;  [numT]
;;  [objT (class-name : symbol)]
;;  [nullT]) ;;null for Q4

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define find-classT
  (make-find classT-name))

(define find-fieldT
  (make-find fieldT-name))

(define find-methodT
  (make-find methodT-name))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'object)
      empty        
      (type-case ClassT (find-classT class-name t-classes)
        [classT (name super-name fields methods)
                (append 
                 (get-all-field-types super-name t-classes)
                 (map fieldT-type fields))])))

;; ----------------------------------------

(define (make-find-in-tree find-in-list extract)
  (lambda (name t-class t-classes)
    (local [(define items (extract t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'object)
          (find-in-list name items)
          (try (find-in-list name items)
               (lambda ()
                 ((make-find-in-tree find-in-list extract)
                  name 
                  (find-classT super-name t-classes)
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree find-fieldT classT-fields))

(define find-method-in-tree
  (make-find-in-tree find-methodT classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name fields methods)
               (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (is-subclass? name1 name2 t-classes)]
            ;;[nullT () true]
            [else false])]
    [nullT () ;;Q4
           (type-case Type t2
             [nullT () true]
             [objT (name) true]
             [else false])]
    [arrayT (type1) ;;Q7
            (type-case Type t2
              [arrayT (type2)
                      (is-subtype? type1 type2 t-classes)]
              [else false])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (classT 'a 'object empty empty))
  (define b-t-class (classT 'b 'a empty empty))

  (test (is-subclass? 'object 'object empty)
        true)
  (test (is-subclass? 'a 'b (list a-t-class b-t-class))
        false)
  (test (is-subclass? 'b 'a (list a-t-class b-t-class))
        true)
  
  ;;Test for Q4
  (test (is-subtype? (nullT) (objT 'object) empty) true)
  (test (is-subtype? (objT 'object) (nullT) empty) false)
  (test (is-subtype? (numT) (nullT) empty) false)

  ;;Test for Q7
  (test (is-subtype? (arrayT (objT 'b)) (arrayT (objT 'a)) (list a-t-class b-t-class))
        true)
  (test (is-subtype? (arrayT (objT 'a)) (arrayT (objT 'b)) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (arrayT (objT 'a)) (arrayT (objT 'a)) (list a-t-class b-t-class))
        true)
  (test (is-subtype? (arrayT (objT 'a)) (numT) (list a-t-class b-t-class))
        false)
  
  (test (is-subtype? (numT) (numT) empty)
        true)
  (test (is-subtype? (numT) (objT 'object) empty)
        false)
  (test (is-subtype? (objT 'object) (numT) empty)
        false)
  (test (is-subtype? (objT 'a) (objT 'b) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (objT 'b) (objT 'a) (list a-t-class b-t-class))
        true))

;; ----------------------------------------

(define typecheck-expr : (ExprI (listof ClassT) Type Type -> Type)
  (lambda (expr t-classes arg-type this-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes arg-type this-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [numT ()
                      (type-case Type (recur r)
                        [numT () (numT)]
                        [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExprI expr
        [numI (n) (numT)]
        [plusI (l r) (typecheck-nums l r)]
        [multI (l r) (typecheck-nums l r)]
        [argI () arg-type]
        [thisI () this-type]
        [nullI () (nullT)]
        [newI (class-name exprs)
              (local [(define arg-types (map recur exprs))
                      (define field-types
                        (get-all-field-types class-name t-classes))]
                (if (and (= (length arg-types) (length field-types))
                         (foldl (lambda (b r) (and r b))
                                true
                                (map2 (lambda (t1 t2) 
                                        (is-subtype? t1 t2 t-classes))
                                      arg-types
                                      field-types)))
                    (objT class-name)
                    (type-error expr "field type mismatch")))]
        [getI (obj-expr field-name)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (find-field-in-tree field-name
                                                    t-class
                                                    t-classes))]
                        (type-case FieldT field
                          [fieldT (name type) type]))]
                [nullT () (error 'typecheck-expr "null reference")] ;;Added for Q4
                [else (type-error obj-expr "object")])]
        ;;Q5
        [setI (obj-expr field-name arg-expr)
              (local [(define obj-type (recur obj-expr))
                      (define arg-type (recur arg-expr))]
                (type-case Type obj-type
                  [objT (class-name)
                        (local [(define t-class
                                  (find-classT class-name t-classes))
                                (define field
                                  (find-field-in-tree field-name
                                                      t-class
                                                      t-classes))]
                          (type-case FieldT field
                            [fieldT (name type)
                                    (if (is-subtype? arg-type type t-classes) 
                                        arg-type
                                        (error 'typecheck-expr "is not subtype"))]))]
                  [nullT () (error 'typecheck-expr "null reference")]
                  [else (type-error obj-expr "object")]))]
        ;;For Q7 ---------------------------------------------------
        [newarrayI (type size init-val)
                   (local [(define sz (recur size))
                           (define init (recur init-val))]
                     (type-case Type sz
                       [numT ()                     
                             (if (is-subtype? init type t-classes)
                                 (arrayT type)
                                 (error 'typecheck-expr "is not subtype"))]
                       [else (type-error sz "number")]))]
        [arrayrefI (arr-of index)
                   (local [(define arr (recur arr-of))
                           (define idx (recur index))]
                     (type-case Type arr
                       [arrayT (type)
                               (type-case Type idx
                                 [numT () type]
                                 [else (type-error idx "number")])]
                       [else (type-error arr "array-of-<Type>")]))]
        [arraysetI (arr-of index new-val)
                   (local [(define arr (recur arr-of))
                           (define idx (recur index))
                           (define n-val (recur new-val))]
                     (type-case Type arr
                       [arrayT (type)
                               (type-case Type idx
                                 [numT ()
                                       (if (is-subtype? n-val type t-classes)
                                           (numT)
                                           (error 'typecheck-expr "is not subtype"))]
                                 [else (type-error idx "number")])]
                       [else (type-error arr "array-of-<Type>")]))]
        ;;Q7 ends here ---------------------------------------------
        [sendI (obj-expr method-name arg-expr)
               (local [(define obj-type (recur obj-expr))
                       (define arg-type (recur arg-expr))]
                 (type-case Type obj-type
                   [objT (class-name)
                         (typecheck-send class-name method-name
                                         arg-expr arg-type
                                         t-classes)]
                   [nullT () (error 'typecheck-expr "null reference")]
                   [else
                    (type-error obj-expr "object")]))]
        [superI (method-name arg-expr)
                (local [(define arg-type (recur arg-expr))
                        (define this-class
                          (find-classT (objT-class-name this-type)
                                       t-classes))]
                  (typecheck-send (classT-super-name this-class)
                                  method-name
                                  arg-expr arg-type
                                  t-classes))]))))

(define (typecheck-send [class-name : symbol]
                        [method-name : symbol]
                        [arg-expr : ExprI]
                        [arg-type : Type]
                        [t-classes : (listof ClassT)])
  (type-case MethodT (find-method-in-tree
                      method-name
                      (find-classT class-name t-classes)
                      t-classes)
    [methodT (name arg-type-m result-type body-expr)
             (if (is-subtype? arg-type arg-type-m t-classes)
                 result-type
                 (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (listof ClassT)]) : ()
  (type-case MethodT method
    [methodT (name arg-type result-type body-expr)
             (if (is-subtype? (typecheck-expr body-expr t-classes
                                              arg-type this-type)
                              result-type
                              t-classes)
                 (values)
                 (type-error body-expr (to-string result-type)))]))

(define (check-override [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (listof ClassT)])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree (methodT-name method)
                                  (find-classT super-name t-classes)
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string (methodT-name method)))))))

(define (typecheck-class [t-class : ClassT] [t-classes : (listof ClassT)])
  (type-case ClassT t-class
    [classT (name super-name fields methods)
            (map (lambda (m)
                   (begin
                     (typecheck-method m (objT name) t-classes)
                     (check-override m t-class t-classes)))
                 methods)]))

(define (typecheck [a : ExprI] [t-classes : (listof ClassT)]) : Type
  (begin
    (map (lambda (t-class)
           (typecheck-class t-class t-classes))
         t-classes)
    (typecheck-expr a t-classes (numT) (objT 'bad))))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (classT 'posn 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))

  (define posn3D-t-class 
    (classT 'posn3D 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define square-t-class 
    (classT 'square 'object
            (list (fieldT 'topleft (objT 'posn)))
            (list)))
  ;;Added new class for test
  (define square2-t-class 
    (classT 'square2 'object
            (list (fieldT 'x (objT 'posn3D)))
            (list)))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class square2-t-class))) ;;Appended new class
  
  (define posn27 (newI 'posn (list (numI 2) (numI 7))))
  (define posn531 (newI 'posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI posn531 'addDist posn27))
        (numT))  
  (test (typecheck-posn (sendI posn27 'addDist posn531))
        (numT))
  (test (typecheck-posn (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
        (objT 'square))
  (test (typecheck-posn (newI 'square (list (newI 'posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))
  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI posn27 'mdist posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class 
                             (classT 'other 'posn
                                     (list)
                                     (list (methodT 'mdist 
                                                    (objT 'object) (numT)
                                                    (numI 10))))))
            "bad override")
  (test/exn (typecheck-method (methodT 'm (numT) (objT 'object) (numI 0)) (objT 'object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'm (numI 0)))))))
            "not found")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'mdist (nullI)))))))
            "not found") ;;test for Q4
  (test/exn (typecheck-posn (sendI (nullI) 'mdist (numI 0)))
            "null reference") ;;test for Q4
  (test/exn (typecheck-posn (getI (nullI) 'x))
            "null reference") ;;test for Q4

  ;;Test for Q5
  (test (typecheck-posn (setI posn27 'x (numI 1)))
        (numT))
  (test (typecheck-posn (setI posn531 'x (numI 1)))
        (numT))
  (test/exn (typecheck-posn (setI (numI 1) 'x (numI 1)))
            "object")
  (test/exn (typecheck-posn (setI posn531 'k (numI 1)))
            "not found")
  (test/exn (typecheck-posn (setI (nullI) 'k (numI 1)))
            "null reference")
  (test/exn (typecheck-posn (setI (newI 'square2 (list posn531)) 'x posn27))
            "is not subtype")
 
  ;;Tests for Q7 and Q10
  (test (typecheck (newarrayI (numT) (numI 3) (numI 1)) empty)
        (arrayT (numT)))
  (test (typecheck (arrayrefI (newarrayI (numT) (numI 3) (numI 1)) (numI 1)) empty)
        (numT))
  (test (typecheck (arraysetI (newarrayI (numT) (numI 3) (numI 1)) (numI 1) (numI 12)) empty)
        (numT))
  (test/exn (typecheck (newarrayI (numT) (nullI) (numI 1)) empty)
            "number")
  (test/exn (typecheck (newarrayI (arrayT (numT)) (numI 3) (numI 1)) empty)
            "is not subtype")
  (test/exn (typecheck (arrayrefI (newarrayI (numT) (numI 3) (numI 1)) (nullI)) empty)
            "number")
  (test/exn (typecheck (arrayrefI (numI 1) (numI 1)) empty)
            "array-of-<Type>")
  (test/exn (typecheck (arraysetI (newarrayI (numT) (numI 3) (numI 1)) (nullI) (numI 12)) empty)
            "number")
  (test/exn (typecheck (arraysetI (numI 1) (numI 2) (numI 12)) empty)
            "array-of-<Type>")
  (test/exn (typecheck-posn (arraysetI
                             (newarrayI (arrayT (objT 'posn3D)) (numI 10) (newarrayI (objT 'posn3D) (numI 3) posn531))
                             (numI 5)
                             (newarrayI (arrayT (objT 'posn)) (numI 7) posn27)))
            "is not subtype")
  (test (typecheck-posn (arraysetI
                         (newarrayI (arrayT (objT 'posn)) (numI 10) (newarrayI (objT 'posn) (numI 3) posn27))
                         (numI 5)
                         (newarrayI (objT 'posn3D) (numI 7) posn531)))
        (numT))
  (test (typecheck-posn (arraysetI
                         (newarrayI (arrayT (objT 'posn)) (numI 10) (newarrayI (objT 'posn) (numI 3) posn27))
                         (numI 5)
                         (newarrayI (objT 'posn) (numI 7) posn27)))
        (numT))) ;;Tests end here

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [classT (name super-name fields methods)
              (classI
               name 
               super-name
               (map fieldT-name fields)
               (map (lambda (m)
                      (type-case MethodT m
                        [methodT (name arg-type result-type body-expr)
                                 (methodI name body-expr)]))
                    methods))])))

(define interp-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map strip-types t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI posn531 'addDist posn27))
        (numV 18))
  (test (interp-t-posn (sendI posn27 'addDist posn531))
        (numV 18)))

