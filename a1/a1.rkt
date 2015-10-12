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
(define dialogue "dialogue")

(define personaTerms '("vile" "villainous" "wicked" "naughty" "blackhearted" "shameless" "scoundrelous"))
(define expressionTerms '("join'd with" "entranc'd by"))
(define nameTerms '("I" "me" "Me" "myself" "Myself"))

(define (countKeyWords mode line)
    (cond
          [(string=? personae mode)
              (if (eq? empty line)
                  0
                  (if (eq? #f (member (first line) bad-words))
                      (countKeyWords mode (rest line))
                      (+ 1 (countKeyWords mode (rest line)))
                  )
               )
          ]
          [(string=? settings mode)
               0]
          [(string=? dialogue mode)
               0]
     )
)

;evalutes the values of each line
(define (evaluateLine mode textLine)
     (cond
          [(string=? personae mode)
               (define pCounter (countKeyWords mode textLine))
               (if (> pCounter 0)
                   (* (* (expt 2 pCounter) -1) (length textLine))
                   (length textLine)
               )
          ]
          [(string=? settings mode)
               0]
          [(string=? dialogue mode)
               0]
     )
)

;evaluate each body of text
(define (evaluateText mode textList)
     (cond
          [(string=? personae mode)
               (if (eq? empty textList)
                   '()
                   (cons (evaluateLine mode (string-split (first textList))) (evaluateText personae (rest textList)))
               )
          ]
          [(string=? settings mode)
               0]
          [(string=? dialogue mode)
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
)

;removes the first word from a string
(define (removeFirstWord textList)
    (if (eq? empty textList)
       '()
       (cons (string-join (rest (string-split (first textList))) " ") (removeFirstWord (rest textList)))
    )
)

;retrieves the first word from a string
(define (cleanString mode textList)
    (cond
        [(string=? personae mode)
           (if (eq? empty textList)
               '()
               (cons (string-trim (first (string-split (first textList))) ",") (cleanString mode (rest textList)))
           )
        ]
        [(string=? settings mode)
             0]
        [(string=? "dialogue-words" mode)
           (if (eq? empty textList)
               '()
               (if (string=? ":" (substring (first textList) (- (string-length (first textList)) 1)))
                   (cleanString mode (rest textList))
                   (cons (first textList) (cleanString mode (rest textList)))
               )
           )
        ]
        [(string=? "dialogue-names" mode)
           (if (eq? empty textList)
               '()
               (if (string=? ":" (substring (first textList) (- (string-length (first textList)) 1)))
                   (cons (string-trim (first textList) ":") (cleanString mode (rest textList)))
                   (cleanString mode (rest textList))
               )
           )
        ]
    )
)

;from ex1
;Finds whether a list of single word strs contains given word
(define (contains-word strlst word)
               (if (empty? strlst)
                          #f
                          (or (equal? (first strlst) word) (contains-word (rest strlst) word))))
;from ex1 rets a lst containing only the strs that contain wrd
(define (filter-by-word strlst wrd)
  (if (empty? strlst)
      empty
      (append 
       (if (equal? (contains-word (string-split (first strlst)) wrd) #t)
           (list(first strlst))
           empty)
           (filter-by-word ( rest strlst) wrd ))))
;same as above but for calls
(define (filter-by-call strlst wrd)
  (if (empty? strlst)
      empty
      (append 
       (if (equal? (contains-word (string-split (first strlst)) wrd) #t)
           (list(first strlst))
           empty)
           (filter-by-word ( rest strlst) wrd ))))

;given sub is prevent in lst
;returns a list of #t and #f indicating whether the corresponding element of lst
;is the first element of any occurrence of sub in lst
(define (sublist-present-list sub lst)
  (if (empty? lst)
      empty
  (append
   (list (equal? (if (>= (length lst) (length sub)) 
              (reverse (list-tail (reverse lst) (-(length lst) (length sub))))
              #f) sub)) 
     (sublist-present-list sub (rest lst)))))
;returns #t if sub is present in lst and #f otherwise
(define (sublist-present sub lst)
  (if (empty? lst)
      #f
  (or
   (equal? (if (>= (length lst) (length sub)) 
              (reverse (list-tail (reverse lst) (-(length lst) (length sub))))
              #f) sub) 
     (sublist-present sub (rest lst)))))

;locates the index +1 of the last occurrence of sub in lst
(define(sublist-index sub lst)
  (if (empty? lst)
      0
      (+(if (sublist-present sub lst) 1 0 ) (sublist-index sub (rest lst)))))

;locates the index of the first #t in a list of true and false values
(define(locate-index trueFalseLst)
  (-(length trueFalseLst) (sublist-index(list #t)(reverse trueFalseLst))))

(define(sublist sub lst)
  ;sub is empty
  (cond [(empty? sub) 0]
        ;sub not in lst
        [else (if (sublist-present sub lst)
            (locate-index(sublist-present-list sub lst))
            #f)]))

;helper function for counting "bad" adjectives

;evaluate the number for an expression without arithmetic 
(define(evaluate-number str-lst)
  ;count the number of "bad" adjectives
  (define num-bad (apply + (map length (map filter-by-word (make-list (length bad-words)str-lst) bad-words))))
  (if (equal? num-bad 0)
      (length str-lst)
      (*(*(expt 2 num-bad) -1) (length str-lst))
      ))

;returns trucate list at end returns a sublist of original lst
(define (index-to-list lst end)
  (if (< end 0)
      empty 
      (cons (first lst) (index-to-list(rest lst) (- end 1)))))

;returns truncated list where returned list starts from index and goes to the end
(define (start-index-lst lst start)
  (reverse (index-to-list (reverse lst) (- (- (length lst) start) 1))))

;returns func evaluating add or mult where op-name is add or mult and operation is + or *
;left-exp is expression on the left hand side of op if #f is entered will be found automatically same for right-exp
(define (perform-arithmetic op-name operation left-exp right-exp str-lst)
  (define op-index (sublist (string-split op-name) str-lst))
  ;returns fuction that calculates given arithmetic operation
  (cond [(equal? #f (or left-exp right-exp))
         (lambda(str-list)
  (operation (evaluate-number(index-to-list str-list (- op-index 1))) (evaluate-number(start-index-lst str-list (+ op-index 2)))))]
        [(equal? #f left-exp)
      (lambda(str-list)
  (operation (evaluate-number(index-to-list str-list (- op-index 1))) right-exp))]
        [(equal? #f right-exp)
          (lambda(str-list)
            (print "this is the op-index")
            (print op-index)
  (operation left-exp (evaluate-number(start-index-lst str-list (+ op-index 2)))))]
  [else
   (lambda(str-list)
  (operation left-exp right-exp))]))

;goes through a nested list, haystack and returns the list with the first element that equal to needle
(define (find-by-first needle haystack)
  (equal? (first haystack) needle))

;strips the name for settings
(define(remove-name namefuncstr-lst)
  (if (empty? namefuncstr-lst )
      empty
   (append (list (apply string-join (list (start-index-lst(string-split (first namefuncstr-lst) )1 )))) (remove-name (rest namefuncstr-lst)))))


;EVALUATES THE ARITHMETIC YOOOO GARY IF YOU NEED to evaluate"join'd with" or "entranc'd by" USE THIS!!!!!
;if there is no arithmetic op in given str-lst will return the evaluated number 
;Evaluates a string with arithmetic by seting the correct op for perform-arthmetic 
(define(evaluate-arithmetic str-lst)

  (if (equal?(or (sublist (string-split add) str-lst) (sublist (string-split mult) str-lst)) #f) ;no arithmetic op
      (evaluate-number str-lst)
  (if(sublist (string-split add) str-lst)
     ((perform-arithmetic add + #f #f str-lst) str-lst)
     ((perform-arithmetic mult * #f #f str-lst) str-lst)
     )
  ))
(define (if-call-func str-lst)
(and (equal?(equal?(sublist (string-split call) str-lst) #f)#f) 
                  (equal? (list-ref str-lst (+ (sublist (string-split call) str-lst) 4)) "and")))
;creates the function for a single string instruction used for each setting  
    (define (setting-func settingsFuncs-lst settingsNames-lst str-lst)
      (if (or (and (empty? settingsFuncs-lst) (empty? settingsNames-lst)) (not(if-call-func str-lst)))
             (cond
        [(sublist (list param) str-lst)
         (cond[(equal? param string)
               ;if input string is just param
               (lambda(x)(x))]
              [  (if (equal? (sublist (list param) str-lst) 0)
                   (cond [(equal? (start-index-lst (index-to-list str-lst 2)1) (string-split add))
                       ;add 
                       (lambda(x)((perform-arithmetic add + x #f str-lst)str-lst))]
                       ;mult
                       [ (equal? (start-index-lst (index-to-list str-lst 2)1) (string-split mult))
                       (lambda(x)((perform-arithmetic mult * x #f str-lst)str-lst))])
                   (if (equal? (index-to-list (start-index-lst str-lst (-(length str-lst) 3))1) (string-split add))
                       ;add 
                       (lambda(x)((perform-arithmetic add + #f x str-lst)str-lst))
                       ;mult
                       (lambda(x)((perform-arithmetic mult * #f x str-lst)str-lst))  ;works
                   
         ))])]
         [else  (lambda(x)(evaluate-arithmetic str-lst)) ];this is a function with no param just a hardcoded ret value
  )
  (cond [(if-call-func str-lst)
        ;find the called function
         (define (find-by-first-set haystack)
           (print "this is the str-lst")
           (print str-lst)
  (equal? (first haystack) (list-ref str-lst (+(sublist (string-split call) str-lst)3)) ))
         ;THIS IS A POOR HACK FIX ITTTTTTT 
;(second (first (filter find-by-first-set settingsFuncs-lst)))
         (print (second(first (filter find-by-first-set settingsFuncs-lst))))
         (lambda(x)
         ((second(first (filter find-by-first-set settingsFuncs-lst)))
          ((setting-func empty empty (start-index-lst str-lst 5)) x))
          )]
        
        [(sublist (list param) str-lst)
         (cond[(equal? param string)
               ;if input string is just param
               (lambda(x)(x))]
              [  (if (equal? (sublist (list param) str-lst) 0)
                   (cond [(equal? (start-index-lst (index-to-list str-lst 2)1) (string-split add))
                       ;add 
                       (lambda(x)((perform-arithmetic add + x #f str-lst)str-lst))]
                       ;mult
                       [ (equal? (start-index-lst (index-to-list str-lst 2)1) (string-split mult))
                       (lambda(x)((perform-arithmetic mult * x #f str-lst)str-lst))])
                   (if (equal? (index-to-list (start-index-lst str-lst (-(length str-lst) 3))1) (string-split add))
                       ;add 
                       (lambda(x)((perform-arithmetic add + #f x str-lst)str-lst))
                       ;mult
                       (lambda(x)((perform-arithmetic mult * #f x str-lst)str-lst))  ;works
                   
         ))])]
         [else  (lambda(x)(evaluate-arithmetic str-lst)) ];this is a function with no param just a hardcoded ret value
  )
      ;check if call
      ))
;helper used to first split strings then run setting-func
(define (split-and-set settingsFuncs-lst settingsNames-lst string)
  (setting-func settingsFuncs-lst settingsNames-lst (string-split string)))
(define (check-not-empty match)
      (not (empty? match)))

(define (evaluate body)
    ;a list of strings that is seperated into Personae, Settings and Dialogue
  
    (define parsedText (bodyParser body))   

    ;'allCharactersNames' and 'allCharactersValues' are two lists of names and values from Persona
    (define allCharactersName (cleanString personae (first parsedText)))   
    (define allCharactersValues (evaluateText personae (removeFirstWord (first parsedText))))

    ;outputDialogueNames and outputDialogueStrings are two lists of names and string from Dialogue
    (define outputDialogueNames (cleanString "dialogue-names" (last parsedText)))
    (define outputDialogueStrings (cleanString "dialogue-words" (last parsedText)))
    
    ;fill in values for settingsName
    (define settingsNames (cleanString personae (second (bodyParser body))))
    (print settingsNames)
  (print (second parsedText))
    (define settingsFuncString( remove-name (second parsedText)))
    ;find all non-call func
        (define (find-call settingsFuncString-lst)
     (if (empty? settingsFuncString-lst)
      empty
    (append (if(and (equal?(equal?(sublist (string-split call) (string-split (first settingsFuncString-lst))) #f)#f) 
                  (equal? (list-ref (string-split (first settingsFuncString-lst)) (+ (sublist (string-split call) (string-split (first settingsFuncString-lst))) 4)) "and"))
    (list #t) (list #f)) (find-call(rest settingsFuncString-lst)))))
  
    ;match names
    ;need func takes in 3 lists name, funcstrs, t-f-list
    (define(matcher match name funcstr true-or-false)
        (if (equal? true-or-false match)
             (list name funcstr ) empty))
    (define (matcher-true name funcstr true-or-false)
        (matcher #t name funcstr true-or-false))
    (define (matcher-false name funcstr true-or-false)
        (matcher #f name funcstr true-or-false))
  (print settingsFuncString)
    ;non-calls
    (define settingsFunc-nocall
      (filter check-not-empty (map matcher-false settingsNames
                         (map split-and-set
                              (make-list (length settingsFuncString) empty)
                              (make-list (length settingsFuncString) empty)
                              settingsFuncString) (find-call settingsFuncString))))
  (print settingsFunc-nocall)
  (define settingsFunc-call 
   (if (not (equal? (length settingsFunc-nocall) (length settingsNames)))
  ;calls
      ;(filter list? (map matcher-true settingsNames
            ;a list of function correponding to the evaluated function strs
           ;(map split-and-set
                ;list for functions 
            ;    (make-list (length settingsFunc-nocall)
             ;                              settingsFunc-nocall)
                               ;list for Names
              ;                (make-list (length settingsNames) settingsNames)
 ;                             settingsFuncString)


           (filter check-not-empty (map matcher-true settingsNames
                          ;will only get first one
                         (map split-and-set
                              (make-list (length settingsFuncString) settingsFunc-nocall)
                              ;(print "this is settingfunc-nocall in rep lst")
                              ;(print (make-list (length settingsFuncString) settingsFunc-nocall))
                              (make-list (length settingsFuncString) settingsNames)
                              settingsFuncString) (find-call settingsFuncString)))
            ;generate true or false list

          ;  (find-call settingsFuncString)))
    ;empty)
    empty)
     )
    
    (print settingsFunc-call)
    ;(define settingsFuncs-nocall (map split-and-set (make-list (length settingsFuncString) empty) (make-list (length settingsFuncString) empty) settingsFuncString))
    ;(define settingsFuncs-call (map split-and-set (make-list (length settingsFuncs-nocall) settingsFuncs-nocall ) (make-list (length settingsNames) settingsNames)  settingsFuncString)) 
    ;(print settingsFuncString)
    ;(print outputDialogueNames)
    ;(print settingsNames)
    (print "should be 16")
    (print ((second (first settingsFunc-nocall)) 4))
  (print "should be 20")
  (print ((second(first settingsFunc-call))4))
  ;(print settingsFunc-call)
    ;(print settingsFuncs)
 

  
)


(define s "sample.txt")
(define d "descriptions.txt")
(define a "arithmetic.txt")
(define n "name_lookup.txt")
(define f "functions.txt")