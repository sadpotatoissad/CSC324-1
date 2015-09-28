#| Sample tests for Exercise 1.
Note the use of the "plai" language to use a robust testing framework.

All test expressions are in the form (test <actual> <expected>).

Note: these tests are only a subset of the tests we will run;
you are strongly advised to add to them on your own.
|#
#lang plai
(abridged-test-output #t)

; Import the module for exercise 1. Note that the filename must be "ex1.rkt"!
(require "ex1.rkt")

; Tests for filter-by-words
(test (filter-by-word '("Hello world" "Hello moon" "Goodbye world") "world")
      '("Hello world" "Goodbye world"))
(test (filter-by-word '("Hello world" "whyhellothere" "ello") "hello")
      '())
(test (filter-by-word '() "f")
      '())

; Tests for sublist
(test (sublist '(30 40) '(10 20 30 40 50))
      2)
(test (sublist '(20 30) '(10 20 30 20 30 40 50))
      1)
(test (sublist '(1 2 3) '(5 4 3 2 1))
      #f)
(test (sublist '() '(4 2 10))
      0)