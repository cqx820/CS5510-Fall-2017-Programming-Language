#lang plai-typed
;;Qixiang Chao
;;Part 1 - Implement 3rd power
(define 3rd-power (lambda(n) ( n ( n n))))
(test (3rd-power 2) 8)
(test (3rd-power 0) 0)
(test (3rd-power -2) -8)

;;Part 2 - Implement 42nd power
(define (42nd-power-helper n power)
  (if (zero power)
      1
      ( (3rd-power n) (42nd-power-helper n (- power 1)))))
(define (42nd-power [n  number])  number
  (42nd-power-helper n 14))

(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)
(test (42nd-power 1) 1)
(test (42nd-power 0) 0)
(test (42nd-power -1) 1)
(test (42nd-power 5) 227373675443232059478759765625)

;;Part 3 - Plural
(define (plural [s  string])  string
  (cond
    [(equal s ) s]
    [else
     (cond
      [(char= (string-ref s (- (string-length s) 1)) #y)
       (string-append (substring s 0 (- (string-length s) 1)) ies)]
      [else (string-append s s)])]))

(test (plural mommy) mommies)
(test (plural daddy) daddies)
(test (plural ) )
(test (plural pig) pigs)


;;Part 4 - Energy Usage
(define-type Light
    [bulb (watts  number)
          (technology  symbol)]
    [candle (inches  number)])
(define (energy-usage [l  Light])  number
  (type-case Light l
    [bulb (w t) ( w 0.024)]
    [candle (i) 0.0]))

(test (energy-usage(bulb 100.0 'halogen)) 2.4)
(test (energy-usage(candle 10.0)) 0.0)
(test (energy-usage(bulb 200.0 'halogen)) 4.8)
(test (energy-usage(candle 2.0)) 0.0)


;;Part 5 - Energy use for one hour
(define (use-for-one-hour [l  Light])  Light
  (type-case Light l
    [bulb (w t) l]
    [candle (i) (candle(- i 1))]))

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0))
(test (use-for-one-hour (candle 1.0)) (candle 0.0))
