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
(define settingsFuncNames '())
(define allCharactersName '())   
(define allCharactersValues '())
(define outputDialogueNames '())

(define arithmeticNames (list add mult))

;splits text into three bodies: personae, Settings and dialogue
(define (bodyParser body)
  (define dPersonae (rest (reverse (list-tail (reverse body) (length (member finis body))))))
  (define settingBody (list-tail body (+ (length dPersonae) 2)))
  (define set
    (if (string=? settings (first (list-tail body (+ (length dPersonae) 2))))
        (rest (reverse (list-tail (reverse settingBody) (length (member finis settingBody)))))
        '()))
  (define dialogue '())
  (if (eq? 0 (length set))
      (set! dialogue settingBody)
      (set! dialogue (list-tail settingBody (+ (length set) 2))))
  (list dPersonae set dialogue))


;evaluates the value of a given line

(define (sublist sub lst)
  (if (eq? sub '())
      0
      (if (member (first sub) lst)
          (if (checkRest sub (member (first sub) lst))
              (- (length lst) (length (member (first sub) lst)))
              (if (eq? (sublist sub (rest(member (first sub) lst))) #f)
                  #f
                  (+  (- (length lst) (length (member (first sub) lst)))
                      (sublist sub (rest(member (first sub) lst)))
                      1)))
          #f)))

(define (checkRest sub lst)
  (if (empty? sub)
      #t
      (if (empty? lst)
          #f
          (if (string=? (first sub) (first lst))
              (checkRest (rest sub) (rest lst))
              #f))))


(define (sublistEvaluation mode keywordsList line)
  (cond
    [(string=? "includeFuncCall" mode)
     (if (eq? empty keywordsList)
         0
         (if (eq? 0 (sublist (string-split (first keywordsList)) line))
             (+ 1 (sublistEvaluation mode (rest keywordsList) line))
             (sublistEvaluation mode (rest keywordsList) line)))]
;    [(string=? "arithmeticLocation" mode)
 ;    (pretty-print line)
  ;    (if (eq? empty keywordsList)
   ;      '()
    ;     (cons (sublist (string-split (first keywordsList)) line) (sublistEvaluation mode (rest keywordsList) line)))]
    [(string=? dialogue mode)
     0]))

 
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
     (sublistEvaluation mode settingsFuncNames (string-split line))]
 ;   [(string=? "arithmeticLocation" mode)
 ;    (sublistEvaluation mode arithmeticNames (string-split line))]
    [(string=? dialogue mode)
     (if (eq? empty line)
         0
         (if (eq? #f (member (first line) self-refs))
             (countKeyWords mode (rest line))
             (+ 1 (countKeyWords mode (rest line)))))
     ]))


;(define (calculateValuesWithOtherNames line value name))

(define (calculateValues textLine counter)
  (define addCut (sublist (string-split add) textLine))
  (define multCut (sublist (string-split mult) textLine))
  
  (define selfName (list-ref outputDialogueNames counter))
  (define selfIndex (- (length allCharactersName) (length (member selfName allCharactersName))))
  (define selfValue (list-ref allCharactersValues selfIndex))
 
   (cond
    [(and (eq? #f addCut) (eq? #f multCut))
     (define numOfSelf (countKeyWords dialogue textLine))
     (define initialValue (evaluateLine personae textLine 0))
     (define valueAfterSelf (+ (- initialValue numOfSelf) (* numOfSelf selfValue)))
     valueAfterSelf
     ]
    [(number? addCut)
     (define numOfSelfLeft (countKeyWords dialogue (take textLine addCut)))
     (define initialValueLeft (evaluateLine personae (take textLine addCut) 0))
     (define valueAfterSelfLeft (+ (- initialValueLeft numOfSelfLeft) (* numOfSelfLeft selfValue)))
     (define numOfSelfRight (countKeyWords dialogue (list-tail textLine (+ 2 addCut))))
     (define initialValueRight (evaluateLine personae (list-tail textLine (+ 2 addCut)) 0))
     (define valueAfterSelfRight (+ (- initialValueRight numOfSelfRight) (* numOfSelfRight selfValue)))
     (+ valueAfterSelfLeft valueAfterSelfRight)]
    
    [(number? multCut)
     (define numOfSelfLeft (countKeyWords dialogue (take textLine multCut)))
     (define initialValueLeft (evaluateLine personae (take textLine multCut) 0))
     (define valueAfterSelfLeft (+ (- initialValueLeft numOfSelfLeft) (* numOfSelfLeft selfValue)))
     (define numOfSelfRight (countKeyWords dialogue (list-tail textLine (+ 2 multCut))))
     (define initialValueRight (evaluateLine personae (list-tail textLine (+ 2 multCut)) 0))
     (define valueAfterSelfRight (+ (- initialValueRight numOfSelfRight) (* numOfSelfRight selfValue)))
     
     (pretty-print selfValue)
     (pretty-print (take textLine multCut))
     (pretty-print valueAfterSelfLeft)
     (pretty-print (list-tail textLine (+ 2 multCut)))
     (pretty-print valueAfterSelfRight)
     (* valueAfterSelfLeft valueAfterSelfRight)
     ]))

;evaluates the values of each line
(define (evaluateLine mode textLine counter)
  (cond
    [(string=? personae mode)
     (define pCounter (countKeyWords mode textLine))
     (if (> pCounter 0)
         (* (* (expt 2 pCounter) -1) (length textLine))
         (length textLine))]
    [(string=? "settingsModeEvaluation" mode)
     (define includeHamlet (countKeyWords "settingsHamlet" textLine))
     (define includeFuncCall (countKeyWords "includeFuncCall" textLine))
     (cond
       [(and (= 0 includeHamlet) (= 0 includeFuncCall)) 0]
       [(and (= 1 includeHamlet) (= 0 includeFuncCall)) 1]
       [(and (= 1 includeHamlet) (< 0 includeFuncCall)) 2]
       [(and (= 0 includeHamlet) (< 0 includeFuncCall)) 3])]
    [(string=? "settingsValueEvaluation" mode)
     0]
    [(string=? "dialogueValues" mode)
     (define includeFuncCall (countKeyWords "includeFuncCall" textLine))
     (define tempValue (calculateValues (string-split textLine) counter))
     tempValue
    ]
    [(string=? dialogue mode)
     0]))

;evaluates each body of text
(define (evaluateText mode textList counter)
  (cond
    [(string=? personae mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (string-split (first textList)) 0) (evaluateText mode (rest textList) 0)))]
    [(string=? "settingsModeEvaluation" mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (first textList) 0) (evaluateText mode (rest textList) 0)))]
    [(string=? "settingsValueEvaluation" mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (string-split (first textList)) 0) (evaluateText mode (rest textList) 0)))]
    [(string=? "dialogueValues" mode)
     (if (eq? empty textList)
         '()
         (cons (evaluateLine mode (first textList) counter) (evaluateText mode (rest textList) (+ counter 1))))]
    [(string=? dialogue mode)
     0]))

;removes the first word from a string
(define (removeFirstWord textList)
  (if (eq? empty textList)
      '()
      (cons (string-join (rest (string-split (first textList))) " ") (removeFirstWord (rest textList)))))

;retrieves the first word from a string
(define (cleanString mode textList)
  (cond
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
         (cons (string-append "The song of " (string-trim (first (string-split (first textList))) ",") " and ") (cleanString mode (rest textList))))]))

(define (evaluate body)
  ;a list of strings that is seperated into Personae, Settings and Dialogue
  (define parsedText (bodyParser body))   
  
  ;'allCharactersNames' and 'allCharactersValues' are two lists of names and values from Persona
  (set! allCharactersName (cleanString personae (first parsedText)))   
  (set! allCharactersValues (evaluateText personae (removeFirstWord (first parsedText)) 0))
    
  ;settingsFuncNames and settingsFuncString are two lists of names and string from Settings
  ;settigsFuncStatus represents the mode of each function where
     ; 0 - takes 0 arguements and calls no other functions
     ; 1 - takes 1 arguements and calls no other functions
     ; 2 - takes 1 arguements and call at least one other function
     ; 3 - takes 0 arguements and calls at least one other function
  (set! settingsFuncNames (cleanString settings(second (bodyParser body))))
  (define settingsFuncString (removeFirstWord (second parsedText)))
  (define settigsFuncStatus (evaluateText "settingsModeEvaluation" settingsFuncString 0))
  (define settingsFuncValues (evaluateText "settingsValueEvaluation" settingsFuncString 0))
  
  ;outputDialogueNames and outputDialogueStrings are two lists of names and string from Dialogue
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