#| Sample tests for Exercise 4.
Note the use of the "plai" language to use a more robust
testing framework.

All test expressions are in the form (test <actual> <expected>).

Note: these tests are only a subset of the tests we will run;
you are strongly advised to add to them on your own.
|#
#lang plai
(abridged-test-output #t)

(require "ex4.rkt")

; Basic point class for testing.
(define (Point x y)
  (lambda (msg)
    (cond [(equal? msg "x") x]
          [(equal? msg "y") y]
          [(equal? msg "distance")
           (lambda (other-point)
             (let ([dx (- x (other-point "x"))]
                   [dy (- y (other-point "y"))])
               (sqrt (+ (* dx dx) (* dy dy)))))]
          [else "Unrecognized message!"])))

; Struct point. Note that this doesn't have a "distance" method!
(define (Point-struct x y)
  (lambda (msg)
    (cond [(equal? msg "x") x]
          [(equal? msg "y") y]
          [else "Unrecognized message!"])))

; Objects
(define p1 (Point 2 3))
(define p2 (Point-struct -1 5))
(define p3 (Point-struct 3 8))
(define p4 (Point 4 6))

; Tests
(test (let ([p1* (distance-trait p1)])
        ((p1* "distance-to-self")))
      0)

(test (let* ([p1* (distance-trait p1)]
             [p ((p1* "closer") p2 p3)])
        (p "x"))
      -1)

(test (let* ([p1* (distance-trait p1)]
             [p ((p1* "closer") p2 p3)])
        (p "y"))
      5)

(test (let* ([p1* (distance-trait p1)]
             [p ((p1* "closer") p4 p2)])
        (p "x"))
      4)

(test (let* ([p1* (distance-trait p1)]
             [p ((p1* "closer") p4 p2)])
        (p "y"))
      6)