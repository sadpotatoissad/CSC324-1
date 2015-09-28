#| Sample tests for Exercise 3.
Note the use of the "plai" language to use a more robust
testing framework.

All test expressions are in the form (test <actual> <expected>).

Note: these tests are only a subset of the tests we will run;
you are strongly advised to add to them on your own.
|#
#lang plai

; Import the module
(require "ex3.rkt")
(abridged-test-output #t)

; Tests for make-splitter
(test (let ([f (make-splitter "hello world")])
        (f "this is a hello world kind of party"))
      '(("this" "is" "a") ("kind" "of" "party")))
(test (let ([f (make-splitter "hello world")])
        (f "this is a hello not world kind of party"))
      #f)
(test (let ([f (make-splitter "jello")])
        (f "jello world a"))
      '(() ("world" "a")))
(test (let ([f (make-splitter "jello")])
        (f "yo yo ma jello"))
      '(("yo" "yo" "ma") ()))

; Tests for curry-2
(define (add-2-mult x y) (* (+ 2 x) y))

(test (let ([func (curry-2 add-2-mult)])
        ((func 4) 5))
      30)
(test (let ([func (curry-2 add-2-mult)])
        ((func 5) 4))
      28)


; Tests for curry-n
(define (f w x y) (+ w (* x y)))

(test (let ([func (curry-n f 3)])
        (((func 4) 6) 10))
      64)