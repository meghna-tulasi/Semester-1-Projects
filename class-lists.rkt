;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "04" "class-lists.rkt")

(provide
  felleisen-roster
  shivers-roster
  possible-roster?
  acceptable-felleisen-answer?)


;;DATA-DEFINITIONS:

(define-struct slip (color name1 name2))
;;A Slip is a
;;(make-slip Color String String)
;;Color is one of the yellow or blue slips of Professor Fellesein and Shivers repectively
;;name1 is the first/last name of student 
;;name2 is the first/last name of student 

;;Template
;; slip-fn: Slip -> ??
#;(define (slip-fn s)
    (slip-color s)
    (slip-name1 s)
    (slip-name2 s))

;;A Color is one of:
;;--"yellow"
;;--"blue"

;;Template:
;;color-fn : Color -> ?
#;(define (color-fn c)
    (cond
    [(string=? c "yellow")]
    [(string=? c "blue")]))

;;ListOfSlip
;;Template:
;;los-fn : LOS -> ?
#;(define (los-fn los)
    (cond
      [(empty? los)...]
      [else(..
           (slip-fn (first-los))
           (los-fn (rest los)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;felleisen-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Felleisen's class, without duplication.
;;EXAMPLES: (felleisen-roster (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K."))))
;;            -> (list ((make-slip "yellow" "Wang" "Xi")
;;                      (make-slip "yellow" "Shriram" "K."))
;;STRATEGY: Using template of ListOfSlip

(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else (append
     (felleisen-list (first los) (rest los))
     (felleisen-roster (rest los)))]))

;;TESTS:
(begin-for-test
  (check-equal? (felleisen-roster (list (make-slip "yellow" "Wang" "Xi")
                           (make-slip "blue" "JOnes" "Tom")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K.")))
                (list (make-slip "yellow" "Xi" "Wang")
                      (make-slip "yellow" "Shriram" "K.")) "List Should return two slips of yellow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;felleisen-list : Slip ListOfSlip -> ListOfSlip
;;GIVEN: the first picked up slip and rest of slips in the list
;;RETURNS: the list of slip which will be yellow and not
;;duplicated in terms of first/last name
;;EXAMPLES: (felleisen-list (make-slip "yellow" "Wang" "Xi")
;;                            (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K."))
;; ->(list (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Shriram" "K."))

;;STRATEGY:Using template of ListofSlip
(define (felleisen-list s los)
  (if
   (and (felleisen-slip? (slip-color s))
        (not (duplicate? s los)))
   (list s) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;fellisen-slip? : Color -> Boolean
;;GIVEN: color of slip
;;RETURNS: true only if the color of the slip is yellow
;;EXAMPLES: (fellisen-slip? "yellow") ->true
;;STRATEGY: Using the template of color of the slip

(define (felleisen-slip? c)
  (cond
    [(string=? c "yellow") true]
    [(string=? c "blue") false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;duplicate? : Slip ListOfSlip -> Boolean
;;GIVEN: slip and list of slip
;;RETURNS: true if listofslip contains picked up slip
;;EXAMPLES: (duplicate? (make-slip "yellow" "Wang" "Xi")
;;(list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K.")) -> true
;;STRATEGY: Using the template of ListOfSlip
(define (duplicate? s los)
 (cond
    [(empty? los) false]
    [else(if (check-names? s (first los)) true
             (duplicate? s (rest los)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;possible-roster? : ListtOfSlip -> Boolean
;;GIVEN; list of slips
;;RETURNS: true iff all slips in the list are the same color,
;;and no student is represented twice.
;;EXAMPLE: (possible-roster? (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K.")) -> false
;;STRATEGY: Using the template of ListOfSlip
(define (possible-roster? los)
  (cond
    [(empty? los) true]
    [(no-duplicacy? (first los) (rest los)) 
    (possible-roster? (rest los))]
    [else false]))
    
;;TESTS:
(begin-for-test
  (check-equal? (possible-roster? (list (make-slip "yellow" "Wang" "Xi")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K."))) false)

  (check-equal? (possible-roster? (list (make-slip "yellow" "Wang" "Xi")
                            (make-slip "blue" "Shriram" "K."))) false)

  (check-equal? (possible-roster? (list (make-slip "yellow" "Wang" "Xi")
                            (make-slip "yellow" "Shriram" "K."))) true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;no-duplicacy? : Slip ListOfSlip -> Boolean
;;GIVEN: slip and list of slip
;;RETURNS: true if there is no duplicate slips in terms of arrangement of names
;;STRATEGY: Using template of ListOfSlip
(define (no-duplicacy? s los)
  (cond
    [(empty? los) true]
    [(check-possibility? s (first los))
      (no-duplicacy? s (rest los))]
    [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-possibility? : Slip Slip  -> Boolen
;;GIVEN : slips
;;RETURNS: true if both both have similar color an differnt names
;;EXAMPLES: (check-possibilty? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Shriram" "K.")))
;;-> true
;;STRATEGY: Combining simple functions
(define (check-possibility? slip1 slip2)
  (and (check-color? slip1 slip2)(different-names? slip1 slip2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;different-names?: Slip Slip -> Boolean
;;GIVEN: names to check if they are  similar on slips
;;RETURNS: true if the name on slip does not match
;;EXAMPLES:(different-names? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Shriram" "K."))
;;true
;;STRATEGY: Using template of slip
(define (different-names? sname1 sname2)
  (cond
    [(different-name1? sname1 sname2) false]
    [(different-name2? sname1 sname2) false]
    [else true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;different-name1?: Slip Slip -> Boolean
;;GIVEN: names to check if they are not similar on slips
;;RETURNS: false if the name on slip does not match
;;STRATEGY: Using template of slip

(define (different-name1? sname1 sname2)
   (and (string=? (slip-name1 sname1) (slip-name1 sname2))
        (string=? (slip-name2 sname1) (slip-name2 sname2))))

;;different-name2?: Slip Slip -> Boolean
;;GIVEN: names to check if they are not similar on slips
;;RETURNS: false if the name on slip does not match
;;STRATEGY: Using template of slip  
(define (different-name2? sname1 sname2)
     (and (string=? (slip-name1 sname1) (slip-name2 sname2))
        (string=? (slip-name2 sname1) (slip-name1 sname2))))

;;TEST:
(begin-for-test
  (check-equal? (different-names? (make-slip "yellow" "Wang" "Xi")
                            (make-slip "yellow" "Shriram" "K."))
#true)
 (check-equal? (different-names? (make-slip "yellow" "Wang" "Xi")
                            (make-slip "yellow" "Wang" "Xi"))
#false)
(check-equal? (different-names? (make-slip "yellow" "Wang" "Xi")
                            (make-slip "yellow" "Xi" "Wang" ))
   
false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-color? : Slip Slip -> Boolean
;;GIVEN : two slips to copare
;;RETURNS: true if  the slips have similar color
;;EXAMPLES: (check-color? (make-slip "yellow" "Shriram" "K.")
  ;;                       (make-slip "yellow" "Xi" "Wang")) -> true
  ;;STRATEGY: Using template of slip
(define (check-color? slip1 slip2)
  (string=? (slip-color slip1) (slip-color slip2)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-names?: Slip Slip -> Boolean
;;GIVEN: names to check if they are similar on slips
;;RETURNS: true if the name on slip matches
;;EXAMPLES: (check-names? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Xi" "Wang"))
;;                -> true
;;STRATEGY: USing template of slip

(define (check-names? sname1 sname2)
(and (string=? (slip-color sname1) (slip-color sname2))
  (or
(and   (string=? (slip-name1 sname1) (slip-name1 sname2))
       (string=? (slip-name2 sname1) (slip-name2 sname2)))
  (and (string=? (slip-name1 sname1) (slip-name2 sname2))
       (string=? (slip-name2 sname1) (slip-name1 sname2))))))

;;TESTS:
(begin-for-test
  (check-equal? (check-names? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Xi" "Wang"))
                true "Both the slips have names of a single student")
(check-equal? (check-names? (make-slip "yellow" "Wang" "Xi") (make-slip "blue" "JOnes" "Tom"))
                false "Both the slips have names of different students")

(check-equal? (check-names? (make-slip "yellow" "Wang" "Xi") (make-slip "yellow" "Wang" "Tom"))
                false "Both the slips have names of different students"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-roster : ListOfSlip -> ListOfSlip
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Shivers's class, without duplication.
;;EXAMPLES: (shivers-roster (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K."))
;;            -> (list ((make-slip "blue" "JOnes" "Tom")))

;;STRATEGY: Using template of ListOfSlip
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else (append
     (shivers-list (first los) (rest los))
     (shivers-roster (rest los)))]))
;; TESTS:
(begin-for-test
  (check-equal? (shivers-roster (list (make-slip "yellow" "Wang" "Xi")
                           (make-slip "blue" "JOnes" "Tom")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K.")))
                (list (make-slip "blue" "JOnes" "Tom"))
                "List Should return one slip of blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-list : Slip ListOfSlip -> ListOfSlip
;;GIVEN: the first picked up slip and rest of slips in the list
;;RETURNS: the list of slip which will be blue and not
;;duplicated in terms of first/last name
;;EXAMPLES: (shivers-list (make-slip "yellow" "Wang" "Xi")
;;                            (list (make-slip "yellow" "Wang" "Xi")
;;                            (make-slip "blue" "JOnes" "Tom")
;;                            (make-slip "yellow" "Xi" "Wang")
;;                            (make-slip "yellow" "Shriram" "K.")))
;; ->(list (make-slip "blue" "JOnes" "Tom"))
;;STRATEGY:Using template of ListofSlip

(define (shivers-list s los)
  (if
   (and (shivers-slip? (slip-color s))
        (not (duplicate? s los)))
   (list s) empty))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;shivers-slip? : Color -> Boolean
;;GIVEN: color of slip
;;RETURNS: true only if the color of the slip is blue
;;EXAMPLES: (shivers-slip? "blue") ->true
;;STRATEGY: Using the template of color of the slip
(define (shivers-slip? c)
  (cond
    [(string=? c "yellow") false]
    [(string=?  c "blue") true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;;GIVEN: two lists of slips, lst1 and lst2
;;RETURNS: true iff every student on a yellow slip in lst1 appears once and only once in lst2.
;;EXAMPLES: (acceptable-felleisen-answer? (list(make-slip "yellow" "Wang" "Xi")
;; (make-slip "yellow" "Shriram" "K."))
;;(list (make-slip "yellow" "Wang" "Xi")
;;(make-slip "blue" "Jones" "Tom")
;;(make-slip "yellow" "Xi" "Wang")
;; (make-slip "yellow" "Shriram" "K."))) -> true
                           
;;STRATEGY: Using ListOfSlip template
(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [(empty? los1) true]
    [(single-existance? (first los1) los2)
     (acceptable-felleisen-answer? (rest los1) los2)]
    [else false]))

;TESTS:
(begin-for-test
  (check-equal? (acceptable-felleisen-answer? (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K.")) (list (make-slip "yellow" "Wang" "Xi")
                                            (make-slip "blue" "Jones" "Tom")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K.")))
#true)
  
(check-equal? (acceptable-felleisen-answer? (list
 (make-slip "yellow" "Shriram" "K.")
 (make-slip "yellow" "Wang" "Xi")) (list (make-slip "yellow" "Wang" "Xi")
                                            (make-slip "blue" "Jones" "Tom")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K.")))
#true)
(check-equal? (acceptable-felleisen-answer? (list
 (make-slip "yellow" "K." "Shriram")
 (make-slip "blue" "Xi" "Wang")) (list (make-slip "yellow" "Wang" "Xi")
                                    (make-slip "blue" "Jones" "Tom")
                            (make-slip "yellow" "Xi" "Wang")
                            (make-slip "yellow" "Shriram" "K.")))
#false)

(check-equal? (acceptable-felleisen-answer? (list (make-slip "yellow" "Wang" "Xi")
                                                 (make-slip "yellow" "Xi" "Wang")) 
  (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))
#true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;single-existance?: Slip ListOfSlip -> Boolean
;;GIVEN: slip, list of slip
;;RETURNS: true only if slip exists once in the list
;;EXAMPLES:(single-existance? (make-slip "blue" "Jones" "Tom")(list
;;              (make-slip "yellow" "Wang" "Xi")
;;              (make-slip "blue" "Jones" "Tom")
;;              (make-slip "yellow" "Xi" "Wang")
;;              (make-slip "yellow" "Shriram" "K."))) -> false

;;STRATEGY:Combining simple functions
(define (single-existance? slip los)
  (and
   (string=? (slip-color slip) "yellow")
   (single-slip? slip los)))

;;TESTS:
(begin-for-test
  (check-equal? (single-existance? (make-slip "blue" "Jones" "Tom")
 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))
#false)
  (check-equal? (single-existance? (make-slip "yellow" "Wang" "Xi")
 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))
#true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;single-slip? : Slip ListOfSlip -> Boolean
;;GIVEN: slip and list of slip
;;RETURNS: true if slip exists once inn list
;;STRATEGY: Using ListOfSlip template
(define (single-slip? slip los)
  (cond
    [(empty? los) false]
    [(slip-match-found? slip (first los)) true]
     [else (single-slip? slip (rest los))]))

;;TESTS:
(begin-for-test
  (check-equal? (single-slip? (make-slip "blue" "Jones" "Tom")
              (list
              (make-slip "yellow" "Wang" "Xi")
             (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K."))) false))
                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;slip-match-found?  Slip Slip -> Boolean
;;GIVEN : two slips to match
;;RETURNS: true only if both the slip match
;;EXAMPLES: (slip-match-found? (make-slip "yellow" "Wang" "Xi")
;;             (make-slip "yellow" "Xi" "Wang"))-> #true
;;STRATEGY: Combining simple functions

(define (slip-match-found? slip1 slip2)
  (same-names? slip1 slip2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;same-names? : Slip SLip -> Boolean
;;GIVEN : two slips to check if the names match
;;RETURNS: true only if the name matches
;;EXAMPLES: (same-names? (make-slip "yellow" "Xi" "Wang")(make-slip "yellow" "Shriram" "K."))->#false
;;STRATEGY : Combining simple functions
(define (same-names? sname1 sname2)
  (cond
    [(different-name2? sname1 sname2) true]
    [(different-name1? sname1 sname2) true]
    [else false]))
  