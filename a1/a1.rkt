#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.


'(("a" "scoundrelous" "and" "vile" "merchant")
  ("a" "charming" "young" "nobleman" "from" "Verona"))

|#



;evalutes the values of each line
(define (evaluateLine mode textList)
  (display textList)
     (define keyTerms '("vile" "villainous" "wicked" "naughty" "blackhearted" "shameless" "scoundrelous"))
     (cond
          [(string=? personae mode)
               (define pCounter 0)
               (for ([i textList])
                    (if (eq? #f (member i keyTerms))
                        '()
                        (set! pCounter (+ pCounter 1))
                    )
               )
            (display pCounter)
            (display "  ")
            (display (length textList))
            (display "\n")
               (if (> pCounter 0)
                   (* (* (expt 2 pCounter) -1) (length textList))
                   (length textList)
               )
               
               ]
          [(string=? settings mode)
               0]
          [(string=? "dialogue" mode)
               0]
     )
)

;evaluate each body of text
(define (evaluateText mode textList)
     (cond
          [(string=? personae mode)
               (define pList '())
               (for ([i textList])
                 (display i)
                 (display "\n")
                    (set! pList (append pList (list (evaluateLine mode (rest (string-split i))))))
               )
               pList
               ]
          [(string=? settings mode)
               0]
          [(string=? "dialogue" mode)
               0]
     )
)

;splits text into three bodies: personae, Settings and dialogue
(define (bodyParser body)
    (define dPersonae (rest (reverse (list-tail (reverse body) (length (member finis body))))))

    (define settingBody (list-tail body (+ (length dPersonae) 2)))
    (define set
         (if (string=? settings (first (list-tail body (+ (length dPersonae) 2))))
             (rest (reverse (list-tail (reverse settingBody) (length (member finis settingBody)))))
             '()
         )
    )

    (define dialogue (list-tail settingBody (+ (length set) 2)))
    (list dPersonae set dialogue)
  ;(list (length dPersonae) (length set) (length dialogue))
)

(define (evaluate body)
    (define parsedText (bodyParser body))
    (evaluateText personae (first parsedText))
  
)

(define s "sample.txt")
(define d "descriptions.txt")
(define a "arithmetic.txt")
(define n "name_lookup.txt")
(define f "functions.txt")