#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors true))

;; ----------------------------------------

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

(define (parse-type [s : s-expression]) : Type ;;Copied from typed-parse
  (cond
    [(s-exp-match? `num s)
     (numT)]
    [(s-exp-match? `SYMBOL s)
     (objT (s-exp->symbol s))]
    ;;[(s-exp-match? `NULL s)
    ;; (nullT)]
    [(s-exp-match? '{array ANY} s) ;;Added for Q7
     (arrayT (parse-type (second (s-exp->list s))))]
    [else (error 'parse-type "invalid input")]))


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
    [(s-exp-match? `null s) (nullI)] ;;null for Q4
    [(s-exp-match? '{+ ANY ANY} s)
     (plusI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{new SYMBOL ANY ...} s)
     (newI (s-exp->symbol (second (s-exp->list s)))
           (map parse (rest (rest (s-exp->list s)))))]
    [(s-exp-match? '{get ANY SYMBOL} s)
     (getI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? '{set ANY SYMBOL ANY} s) ;;parse set for Q5
     (setI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    ;;Parse for Q7
    [(s-exp-match? '{newarray ANY ANY ANY} s)
     (newarrayI (parse-type (second (s-exp->list s)))
                (parse (third (s-exp->list s)))
                (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{arrayref ANY ANY} s)
     (arrayrefI (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    [(s-exp-match? '{arrayset ANY ANY ANY} s)
     (arraysetI (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s)))
                (parse (fourth (s-exp->list s))))]
    
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
  (test (parse `null) (nullI)) ;;Test for Q4
  
  (test (parse `{set null x 1}) ;;Test for Q5
        (setI (nullI) 'x (numI 1)))

  ;;Tests for Q7
  (test (parse '{newarray num 3 1})
        (newarrayI (numT) (numI 3) (numI 1)))
  ;;(test (parse '{newarray null 3 null})
  ;;      (newarrayI (nullT) (numI 3) (nullI)))
  (test (parse '{newarray {array num} 3 1})
        (newarrayI (arrayT (numT)) (numI 3) (numI 1)))
  (test (parse '{arrayref {newarray num 3 1} 2})
        (arrayrefI (newarrayI (numT) (numI 3) (numI 1)) (numI 2)))
  (test (parse '{arrayset {newarray num 3 1} 2 7})
        (arraysetI (newarrayI (numT) (numI 3) (numI 1)) (numI 2) (numI 7)))
  
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
      [objV (class-name field-vals) `object]
      [nullV () `null];;null for Q4
      [arrayV (type size arrof) `array]))) 

(module+ test
  (test (interp-prog empty `null) `null) ;;Test for Q4
  (test (interp-prog ;;Test for Q7
         (list
          '{class arr-test extends object
             {x}
             {new-arr {newarray num 6 arg}}
             {arr-set {arrayset {get this x} arg 10}}
             {arr-ref {arrayref {get this x} arg}}})
         '{send {new arr-test 5} new-arr 1})
        `array)
  
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
        '18)
  
  ;;tests for Q4 ---------------------------
  (test/exn (interp-prog 
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
        
             '{send null addDist {new posn 2 7}})
            "null reference") 
  (test/exn (interp-prog 
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
        
             '{send {new posn3D 5 3 1} addDist null})
            "null reference")) ;;test for Q4

