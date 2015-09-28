#| Sample tests for Exercise 2.
Note the use of the "plai" language to use a more robust
testing framework.

All test expressions are in the form (test <actual> <expected>).

Note: these tests are only a subset of the tests we will run;
you are strongly advised to add to them on your own.
|#
#lang plai
(abridged-test-output #t)

; Import the module
(require "ex2.rkt")


; Tests for apply-functions
(define (id x) x)
(define (return-4 x) (+ 1 3))
(define (add-3 x) (+ x 3))
(define (times-2 x) (* 2 x))

(test (apply-functions (list id return-4) 10)
      '(10 4))


(test (apply-functions '() '(1 2 3))
      '())

(test (apply-functions (list add-3) 1)
      '(4))

(test (apply-functions (list first rest length id) '(1 2 3))
      '(1 (2 3) 3 (1 2 3)))


; Tests for functions-sort
(test (let ([fs (function-sort (list (lambda (x) (+ x 3)) 
                                     (lambda (x) (- 100 x))
                                     (lambda (x) (* x 2)))
                               5)])
        ((first fs) 6))
      9)

(test (let ([fs (function-sort (list (lambda (x) (+ x 3)) 
                                     (lambda (x) (- 100 x))
                                     (lambda (x) (* x 2)))
                               5)])
        ((second fs) 6))
      12)

(test (let ([fs (function-sort (list (lambda (x) (+ x 3)) 
                                     (lambda (x) (- 100 x))
                                     (lambda (x) (* x 2)))
                               5)])
        ((third fs) 6))
      94)