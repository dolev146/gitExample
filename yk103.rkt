#lang pl
#|
Reminder:
Remember that lists are defined inductively as either:
An empty list — null
A cons pair (sometimes called a “cons cell”) of any head value and a list
as its tail — (cons x y)
A “Listof T” would be similar, except that it will use the type T (which
needs to be defined by you, say, Symbol, Natural, or Number) instead of
“any”.

Q1.a 

define a recursive function open-list that consumes a list of lists (where the type of the elements in the inner
lists in a Number) and returns a list contains all the elements of the inner lists concatenated in the same order.
For example, written in a form of a test that you can use: 
((test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)))

Input : List of lists of numbers 
Output : List of numbers 
process : used append function to concatenate the elements of the inner lists , took an 30 minutes . 

|#

(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]))


#|
Q1.b 
Define a function min&max that consumes a list of lists (where the type
of the elements in the inner lists in a Number) and returns a list
containing the minimum and the maximum of the values in the inner
lists.

Here you should use the Racket built-in min/max functions that
consumes numbers as parameters and returns the
maximum/minimum value between them.

Note:the built-in min/max functions returns the
minimum/maximum value in the greater family between the

given numbers, following are some examples:
  - (max 9 3.3) returns 9.0 (and not 9) since Number is
        contains Reals (it contains both floating numbers and
        reals).

  - (min 0.75 1/2) returns 0.5 since Numbers contains
        both floating numbers and exact ratios.
  - (max 1 1/2) returns 1 but as an Exact rational number
        since Numbers contains both floating numbers and exact
        rationals.

- Hint: you may use the default values of the minimum/maximum
  values as +inf.0 / -inf.0 which refers to ±∞.

For example, written in a form of a test that you can use:
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))  

Input : List of lists of numbers
Output : List of numbers
process : used min and max function to find the minimum and maximum of the elements of the inner lists , took 1 hour .

|#

(: min-list : (Listof Number) -> Number)
(define (min-list lst)
  (cond [(null? lst) +inf.0]
        [else (min (first lst) (min-list (rest lst)))]))

(: max-list : (Listof Number) -> Number)
(define (max-list lst)
  (cond [(null? lst) -inf.0]
        [else (max (first lst) (max-list (rest lst)))]))

(: min&max : (Listof (Listof Number)) -> (Listof Number))
(define (min&max lst)
  (list (min-list (open-list lst)) (max-list (open-list lst))))

#|
Q1.c:
To solve the “problem” in part “b” you may want to use the built in
Racket apply, and min/max.
Write a function min&max_apply that does exactly what you did in
part “b” but using apply function.
For example, written in a form of a test that you can use:
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))

Input : List of lists of numbers
Output : List of numbers
process : used apply function to find the minimum and maximum of the elements of the inner lists , took 20 minutes  .
|#

(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (list (apply min (open-list lst)) (apply max (open-list lst))))

;; ----------- Test cases for Q1.a, Q1.b, Q1.c ----------------

;; Q1.a

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '((1 2 3) (4 5 6) (7 8 9))) => '(1 2 3 4 5 6 7 8 9))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) => '(1 2 3 4 5 6 7 8 9 10 11 12))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
(test (open-list '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24) (25 26 27))) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ))
(test (open-list '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10))) => '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10))
(test (open-list '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10) (11 11 11 11))) => '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11))

;; Q1.b

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9))) => '(1.0 9.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) => '(1.0 12.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))) => '(1.0 15.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18))) => '(1.0 18.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21))) => '(1.0 21.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24))) => '(1.0 24.0))
(test (min&max '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24) (25 26 27))) => '(1.0 27.0))
(test (min&max '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10))) => '(1.0 10.0))
(test (min&max '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10) (11 11 11 11))) => '(1.0 11.0))

;; Q1.c

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9))) => '(1 9))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) => '(1 12))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))) => '(1 15))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18))) => '(1 18))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21))) => '(1 21))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24))) => '(1 24))
(test (min&max_apply '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15) (16 17 18) (19 20 21) (22 23 24) (25 26 27))) => '(1 27))
(test (min&max_apply '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10))) => '(1 10))
(test (min&max_apply '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6) (7 7 7 7) (8 8 8 8) (9 9 9 9) (10 10 10 10) (11 11 11 11))) => '(1 11))

#|
Q2 :
In this question we will implement a simple Table data structure. In this data
structure you will need to define a new type called Table. Each element in
the table will be keyed (indexed) with a symbol. In the following the operations
that you are required to implement are detailed below, together with some
guidance.

Q2.1 Implement the empty table EmptyTbl - this should be a variant of the
data type (constructor).

Process : used define to define the empty table , took 5 minutes .

Q2.2 Implement the add operation Add - this too should be a variant of the
data type. The add operation should take as input a symbol (key), a string
(value), and an existing table and return an extended table in the natural
way - see examples below.

Process : used define to define the add operation , took 20 minutes .

Q2.3 Implement the search operation search-table - the search operation
should take as input a symbol (key) and a table and return the first (LIFO,
last in first out) value that is keyed accordingly - see examples below. If
the key does not appear in the original table, it should return a #f value
(make sure the returned type of the function supports this; use the
strictest type possible for the returned type).

Process : used define to define the search operation , took 20 minutes .

Q2.4 Implement the remove item operation remove-item - the remove item
operation should take as input a table and a symbol (key) and return a
new table contains the items of the original table except of the item
to be deleted without the (first (LIFO) keyed value) - see examples
below. If the original table was empty, it should return an empty table
value.

|#

(define-type Table
  [EmptyTbl] ; empty table
  [Add Symbol String Table]) ; add a new item to the table

(: search-table : Symbol Table -> (U String #f)) ; search for a key in the table
(define (search-table keyin table)
         (cases table ; if the key does not appear in the original table, it should return a #f value
           [(EmptyTbl) #f]
           [(Add smbl str tb) (if(eq? keyin smbl) str (search-table keyin tb))] 
         ))
(: remove-item : Table Symbol -> Table) ; remove an item from the table
(define (remove-item table keyin)  
   (cases table
           [(EmptyTbl) (EmptyTbl)] 
           [(Add smbl str tb) 
            (cond[(eq? smbl keyin) tb]
                [else(Add smbl str (remove-item tb keyin))])]
     ))

;; ----------- Test cases for Q2.1, Q2.2, Q2.3, Q2.4 -----------

;; Q2.1

(test (EmptyTbl) => (EmptyTbl))

;; Q2.2

(test (Add 'a "hello" (EmptyTbl)) => (Add 'a "hello" (EmptyTbl)))
(test (Add 'a "hello" (Add 'b "world" (EmptyTbl))) => (Add 'a "hello" (Add 'b "world" (EmptyTbl))))
(test (Add 'a "hello" (Add 'b "world" (Add 'c "!" (EmptyTbl)))) => (Add 'a "hello" (Add 'b "world" (Add 'c "!" (EmptyTbl)))))
(test (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (EmptyTbl))))) => (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (EmptyTbl))))))
(test (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (Add 'e "!" (EmptyTbl)))))) => (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (Add 'e "!" (EmptyTbl)))))))
(test (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (Add 'e "!" (Add 'f "!" (EmptyTbl))))))) => (Add 'a "hello" (Add 'b "world" (Add 'c "!" (Add 'd "!" (Add 'e "!" (Add 'f "!" (EmptyTbl))))))))


;; Q2.3

(test (search-table 'a (EmptyTbl)) => #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "AAA")
(test (search-table 'b (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "B")
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (Add 'c "C" (EmptyTbl))))))=> "AAA")
(test (search-table 'b (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (Add 'c "C" (EmptyTbl))))))=> "B")
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (Add 'c "C" (EmptyTbl))))))=> "C")
(test (search-table 'd (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (Add 'c "C" (EmptyTbl))))))=> #f)



;; Q2.4

(test (remove-item (EmptyTbl) 'b)=> (EmptyTbl))
(test (remove-item (Add 'a "CCC" (Add 'b "BBB" (Add 'a "AAA" (EmptyTbl)))) 'a)=> (Add 'b "BBB" (Add 'a "AAA" (EmptyTbl))))
(test (remove-item (Add 'a "CCC" (Add 'b "BBB" (Add 'a "AAA" (EmptyTbl)))) 'b)=> (Add 'a "CCC" (Add 'a "AAA" (EmptyTbl))))


(test (remove-item (Add 'a "`ToDelete" (EmptyTbl)) 'a)=> (EmptyTbl))



