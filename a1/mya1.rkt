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

;'allCharactersNames' and 'allCharactersValues' are two lists of names and values from Persona
(define allCharactersName '())   
(define allCharactersValues '())

;settingsFuncNames and settingsFuncString are two lists of names and string from Settings
;settingsFuncStatus represents the mode of each function where
; 0 - takes 0 arguements and calls no other functions
; 1 - takes 1 arguements and calls no other functions
; 2 - takes 1 arguements and calls another function
; 3 - takes 0 arguements and calls another function
(define settingsFuncNames '())
(define settigsFuncStatus '())
(define settingsFuncString '())

(define outputDialogueNames '())

;splits text into three bodies: personae, Settings and dialogue
(define (bodyParser body)
  (define dPersonae (rest (reverse (list-tail (reverse body) (length (member finis body))))))
  (define settingBody (list-tail body (+ (length dPersonae) 2)))
  (define set
    (if (string=? settings (first (list-tail body (+ (length dPersonae) 2))))
        (rest (reverse (list-tail (reverse settingBody) (length (member finis settingBody)))))
        '()))
  (define dialogueList '())
  (if (eq? 0 (length set))
      (set! dialogueList settingBody)
      (set! dialogueList (list-tail settingBody (+ (length set) 2))))
  (list dPersonae set dialogueList))

;returns true of given list is a sublist of another list
(define (sublistHelper sub lst)
  (if (empty? sub)
      #t
      (if (empty? lst)
          #f
          (if (string=? (first sub) (first lst))
              (sublistHelper (rest sub) (rest lst))
              #f))))

;finds the index of a the sublist in a list or else returns false
(define (sublist sub lst)
  (if (eq? sub '())
      0
      (if (member (first sub) lst)
          (if (sublistHelper sub (member (first sub) lst))
              (- (length lst) (length (member (first sub) lst)))
              (if (eq? (sublist sub (rest(member (first sub) lst))) #f)
                  #f
                  (+  (- (length lst) (length (member (first sub) lst)))
                      (sublist sub (rest(member (first sub) lst)))
                      1)))
          #f)))

;counts the number of occurences of a keyword list in a given line
(define (sublistEvaluation keywordsList line)
  (if (eq? empty keywordsList)
      0
      (if (eq? 0 (sublist (string-split (first keywordsList)) line))
          (+ 1 (sublistEvaluation (rest keywordsList) line))
          (sublistEvaluation (rest keywordsList) line))))

;evaluates the number of keywords in given a specific mode and a string
(define (countKeyWords mode line)
  (cond
    [(string=? personae mode)
     (if (eq? empty line)
         0
         (if (eq? #f (member (first line) bad-words))
             (countKeyWords mode (rest line))
             (+ 1 (countKeyWords mode (rest line)))))]
    [(string=? "settingsHamlet" mode)
     (if (member param (string-split line))
         1
         0)]
    [(string=? "includeFuncCall" mode)
     (sublistEvaluation settingsFuncNames (string-split line))]
    [(string=? dialogue mode)
     (if (eq? empty line)
         0
         (if (eq? #f (member (first line) self-refs))
             (countKeyWords mode (rest line))
             (+ 1 (countKeyWords mode (rest line)))))]
    [else
     (if (eq? empty line)
         0
         (if (string=? (first line) mode)
             (+ 1 (countKeyWords mode (rest line)))
             (countKeyWords mode (rest line))))]))

;recursively loops through names to update values
(define (calculateValuesWithNames textLine value charactersName charactersValues)
  (cond
    [(eq? empty charactersName)
     value]
    [else
     (define numCharName (countKeyWords (first charactersName) textLine))
     (define newValue (+ (- value numCharName) (* numCharName (first charactersValues))))
     (calculateValuesWithNames textLine newValue (rest charactersName) (rest charactersValues))]))

;update values using name substitution
(define (calculateValuesArthmeticHelper textLine selfValue)
  (define initialValue (evaluateLine personae textLine 0))
  (if (number? (first textLine))
      (first textLine)
      (cond
        [(eq? 1 (length textLine))
         (define numOfSelf (countKeyWords dialogue textLine))
         (define initValue (evaluateLine personae textLine 0))
         (if (eq? 0 numOfSelf)
             (calculateValuesWithNames textLine initValue allCharactersName allCharactersValues)
             selfValue)]
        [else
         (evaluateLine personae textLine 0)])))

;calculate values with arthithemtic operations
(define (calculateValuesArithmetic textLine counter)
  (define addCut (sublist (string-split add) textLine))
  (define multCut (sublist (string-split mult) textLine))
  (define selfValue 0)
  ;augment operations based on if input is from dialogue or from settings
  (cond
    [(eq? -1 counter)
     (set! selfValue 0)]
    [else
     (define selfName (list-ref outputDialogueNames counter))
     (define selfIndex (- (length allCharactersName) (length (member selfName allCharactersName))))
     (set! selfValue (list-ref allCharactersValues selfIndex))])
  ;perform operations as needed
  (cond
    [(and (eq? #f addCut) (eq? #f multCut))
     (calculateValuesArthmeticHelper textLine selfValue)]
    [(number? addCut)
     (define leftValue (calculateValuesArthmeticHelper (take textLine addCut) selfValue))
     (define rightValue (calculateValuesArthmeticHelper (list-tail textLine (+ 2 addCut)) selfValue))
     (+ leftValue rightValue)]
    [(number? multCut)
     (define leftValue (calculateValuesArthmeticHelper (take textLine multCut) selfValue))
     (define rightValue (calculateValuesArthmeticHelper (list-tail textLine (+ 2 multCut)) selfValue))
     (* leftValue rightValue)]))

;update dialogue values with respective function call
(define (calculateValueFunc preFuncValue funcIndex)
  (define funcString (list-ref settingsFuncString funcIndex))
  (cond
    ;function call takes 0 arguements and calls no other functions
    [(eq? 0 (list-ref settigsFuncStatus funcIndex))
     (calculateValuesArithmetic (string-split funcString) -1)]
    
    ;function call takes 1 arguements and calls no other functions
    [(eq? 1 (list-ref settigsFuncStatus funcIndex))
     (define leftList (take (string-split funcString) (- (length (string-split funcString)) (length (member "Hamlet" (string-split funcString))))))
     (define rightList (rest (member "Hamlet" (string-split funcString))))
     (calculateValuesArithmetic (append leftList (list preFuncValue) rightList) -1)]
    
    ;function call takes takes 1 arguements and calls another function
    [(eq? 2 (list-ref settigsFuncStatus funcIndex))
     (define expression (rest (member "and" (string-split funcString))))
     (define subFuncCallList (take (string-split funcString) (- (length (string-split funcString)) (length expression))))
     (define subFuncCall (string-join subFuncCallList " "))

     (define funcIndex (- (length settingsFuncNames) (length (member subFuncCall settingsFuncNames))))
     (define leftList (take expression (- (length expression) (length (member "Hamlet" expression)))))
     (define rightList (rest (member "Hamlet" (string-split funcString))))
     (define preFuncValuePrime (calculateValuesArithmetic (append leftList (list preFuncValue) rightList) -1))
     (calculateValueFunc preFuncValuePrime funcIndex)]
    
    ;function call takes 0 arguements and calls another function
    [(eq? 3 (list-ref settigsFuncStatus funcIndex))
     (define expression (rest (member "and" (string-split funcString))))
     (define subFuncCallList (take (string-split funcString) (- (length (string-split funcString)) (length expression))))
     (define subFuncCall (string-join subFuncCallList " "))
     
     (define funcIndex (- (length settingsFuncNames) (length (member subFuncCall settingsFuncNames))))
     (define preFuncValue (calculateValuesArithmetic expression -1))
     (calculateValueFunc preFuncValue funcIndex)]))

;differentiates and evaluates each line based on different mode of input
(define (evaluateLine mode textLine counter)
  (cond
    ;personae character evaluation
    [(string=? personae mode)
     (define pCounter (countKeyWords mode textLine))
     (if (> pCounter 0)
         (* (* (expt 2 pCounter) -1) (length textLine))
         (length textLine))]
    
    ;settings type evaluation
    [(string=? settings mode)
     (define includeHamlet (countKeyWords "settingsHamlet" textLine))
     (define includeFuncCall (countKeyWords "includeFuncCall" textLine))
     (cond
       ;parses settings into different types based criterion specfied in 'settingsFuncStatus'
       [(and (= 0 includeHamlet) (= 0 includeFuncCall)) 0]
       [(and (= 1 includeHamlet) (= 0 includeFuncCall)) 1]
       [(and (= 1 includeHamlet) (= 1 includeFuncCall)) 2]
       [(and (= 0 includeHamlet) (= 1 includeFuncCall)) 3])]
    
    ;dialogue evaluation
    [(string=? "dialogueValues" mode)
     (define includeFuncCall (countKeyWords "includeFuncCall" textLine))
     (cond
       ;dialogue contains no function call
       [(eq? 0 includeFuncCall)
        (calculateValuesArithmetic (string-split textLine) counter)]
       [else
        (define expression (rest (member "and" (string-split textLine))))
        (define funcCallList (take (string-split textLine) (- (length (string-split textLine)) (length expression))))
        (define funcCall (string-join funcCallList " "))
        (cond
          ;dialogue contains function call that does not exist
          [(eq? #f (member funcCall settingsFuncNames))
           (calculateValuesArithmetic (string-split textLine) counter)]
          ;dialogue contains a valid function call
          [else
           (define funcIndex (- (length settingsFuncNames) (length (member funcCall settingsFuncNames))))
           (define preFuncValue (calculateValuesArithmetic expression counter))
           (calculateValueFunc preFuncValue funcIndex)])])]))

;evaluates all text of a given mode by interpreting it line by line
(define (evaluateText mode textList counter)
  (cond
    [(string=? personae mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (string-split (first textList)) 0) (evaluateText mode (rest textList) 0)))]
    [(string=? settings mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (first textList) 0) (evaluateText mode (rest textList) 0)))]
    [(string=? "dialogueValues" mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (first textList) counter) (evaluateText mode (rest textList) (+ counter 1))))]))

;cleans the formating of a given text for storage
(define (cleanString mode textList)
  (cond
    [(string=? "removeFirstWord" mode)
     (if (eq? empty textList)
         '()
         (cons (string-join (rest (string-split (first textList))) " ") (cleanString mode (rest textList))))]
    [(string=? personae mode)
     (if (eq? empty textList)
         '()
         (cons (string-trim (first (string-split (first textList))) ",") (cleanString mode (rest textList))))]
    [(string=? "dialogueWords" mode)
     (if (eq? empty textList)
         '()
         (if (string=? ":" (substring (first textList) (- (string-length (first textList)) 1)))
             (cleanString mode (rest textList))
             (cons (first textList) (cleanString mode (rest textList)))))]
    [(string=? "dialogueNames" mode)
     (if (eq? empty textList)
         '()
         (if (string=? ":" (substring (first textList) (- (string-length (first textList)) 1)))
             (cons (string-trim (first textList) ":") (cleanString mode (rest textList)))
             (cleanString mode (rest textList))))]
    [(string=? settings mode)
     (if (eq? empty textList)
         '()
         (cons (string-append "The song of " (string-trim (first (string-split (first textList))) ",") " and") (cleanString mode (rest textList))))]))

;evaluates entire text
(define (evaluate body)
  (define parsedText (bodyParser body))   
  
  (set! allCharactersName (cleanString personae (first parsedText)))   
  (set! allCharactersValues (evaluateText personae (cleanString "removeFirstWord" (first parsedText)) 0))
  
  (set! settingsFuncNames (cleanString settings(second (bodyParser body))))
  (set! settingsFuncString (cleanString "removeFirstWord" (second parsedText)))
  (set! settigsFuncStatus (evaluateText settings settingsFuncString 0))

  (set! outputDialogueNames (cleanString "dialogueNames" (last parsedText)))
  (define outputDialogueStrings (cleanString "dialogueWords" (last parsedText)))
  (define outputDialogueValues (evaluateText "dialogueValues" outputDialogueStrings 0))
  
  outputDialogueValues
)

(define z "my.txt")
(define s "sample.txt")
(define d "descriptions.txt")
(define a "arithmetic.txt")
(define n "name_lookup.txt")
(define f "functions.txt")