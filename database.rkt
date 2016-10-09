#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student IDs for each of your group members below.***
<Name>, <CDF>, <ID>
<Name>, <CDF>, <ID>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
;(define (And x y) (and x y)) 
;(define (Or x y) (or x y))
;(define (If x y z) (if x y z))
;
; Correction Oct 5 2016
(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c))))

(define-syntax And
  (syntax-rules ()
    ((And x y)
     (if (equal? #f x)
         #f
         y))))

(define-syntax Or
  (syntax-rules ()
    ((Or a b)
     (if (equal? #t a)
         #t
         b))))

; Please do define And, Or as syntactic forms
; We have actually done this in class you may use the class code and this week's lab code for this.
  

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
       (first table))

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
       (rest table))

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
     (length (rest table)))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (helper1 attrs name tuple) 
  (if (empty? tuple)
      '()
      (if (equal? (first attrs) name)
          (first tuple)
          (helper1 (rest attrs) name (rest tuple)))))


#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

;TESTING FOR HELPER2
(define (test_helper2 tuple)
  (if (equal? (second tuple) 0)
      #t
      #f))



(define (helper2 f table)
  (filter f (tuples table)))


#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#

(define (replace-attr x attrs)
   (lambda (tuple)
     (if (empty? (helper1 attrs x tuple))
         x
         (helper1 attrs x tuple))))



;actual code starts here

;BASIC SELECT HELPERS

;similiar to helper1, but takes a list of names and returns a lst of values matched
(define (helper4 attrs names tuple)
  (if (equal? names "*")
      tuple
      (map (lambda (input)
             (helper1 attrs input tuple)) names)))

;similiar to helper4 but takes a table instead of a tuple
(define (helper5 attrs names table)
  (map (lambda (input) (helper4 attrs names input)) (tuples table)))
                                   
;BASIC SELECT

(define (basic_select names table)
  (helper5 (attributes table) names table))


;MULTIPLE TABLES HELPERS
(define (add lst table)
  (map (lambda (table1) (append lst table1)) table))
         
(define (cartesian-product table1 table2)
        (if (or (empty? table1) (empty? table2)) '()
           (apply append (map (lambda (lst1) (add lst1 table2)) table1))))


;check if 2 lists have the same values, if yes. append the name of the list to that attr
(define (check-duplicate attr1 name attr2)
         (if (empty? attr1) '()
            (if (member (first attr1) attr2)
                (cons (string-append name "." (first attr1)) (check-duplicate (rest attr1) name attr2))
                (cons (first attr1) (check-duplicate (rest attr1) name attr2)))))

(define-syntax change-all
  (syntax-rules ()
    [(change-all ) '()]
    [(change-all p) p]
    [(change-all p q ...)
     (cons )]
    ))

;MULTIPLE TABLES

;FILTERING TUPLES HELPERS
(define (create-unary f name value attrs)
  (lambda(tuple)
    (if (f (helper1 attrs name tuple) value)
        #t
        #f)))
  
;FILTERING TUPLES
(define (filter-tuples table pred)
  (helper2 (create-unary (first pred) (second pred) (third pred) (attributes table)) table))
  

;ORDER BY HELPERS

;ORDER BY


  
; Starter for Part 3; feel free to ignore!



; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))
