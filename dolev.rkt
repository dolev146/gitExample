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

Question:1.a:
define a recursive function open-list that
consumes a list of lists (where the type of the elements in the inner
lists in a Number) and returns a list contains all the elements of the
inner lists concatenated in the same order.
For example, written in a form of a test that you can use:
((test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90)))

|#
;; Answer:1.a: 

(: open-list : Listof (Listof Number) -> (Listof Number))
(define (open-list lst)
  (cond
    [(null? lst) null]
    [else (append (car lst) (open-list (cdr lst)))]))


#|
Question:1.b:

- Define a function min&max that consumes a list of lists (where the type
  of the elements in the inner lists in a Number) and returns a list
  containing the minimum and the maximum of the values in the inner
  lists.

- Here you should use the Racket built-in min/max functions that
  consumes numbers as parameters and returns the
  maximum/minimum value between them.

- Note:the built-in min/max functions returns the
  minimum/maximum value in the greater family between the

- given numbers, following are some examples:
      * (max 9 3.3) returns 9.0 (and not 9) since Number is
        contains Reals (it contains both floating numbers and
        reals).

      * (min 0.75 1/2) returns 0.5 since Numbers contains
        both floating numbers and exact ratios.
      * (max 1 1/2) returns 1 but as an Exact rational number
        since Numbers contains both floating numbers and exact
        rationals.

- Hint: you may use the default values of the minimum/maximum
  values as +inf.0 / -inf.0 which refers to ±∞.

For example, written in a form of a test that you can use:
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))  

|#

;; Answer:1.b: 

;; define two helper functions to find the min and max of a list of numbers
;; using the built-in min/max functions
(: open-list : (Listof (Listof Number)) -> (Listof Number)) ; type annotation for open-list
(define (open-list lst) ; open-list function
  (cond ; cond expression
    [(null? lst) null] ; base case
    [else (append (car lst) (open-list (cdr lst)))])) ; recursive case

(: min-list : (Listof Number) -> Number) ; type annotation for min-list
(define (min-list lst) ; min-list function
  (cond ; cond expression 
    [(null? lst) +inf.0] ; base case
    [else (min (car lst) (min-list (cdr lst)))])) ; recursive case

(: max-list : (Listof Number) -> Number) ; type annotation for max-list
(define (max-list lst) ; max-list function
  (cond ; cond expression
    [(null? lst) -inf.0] ; base case
    [else (max (car lst) (max-list (cdr lst)))])) ; recursive case

;; define the main function
(: min&max : (Listof (Listof Number)) -> (Listof Number)) ; type annotation for min&max
(define (min&max lst) ; min&max function
  (list (min-list (open-list lst)) (max-list (open-list lst)))) ; returns a list containing the min and max of the list of lists



#|
Question:1.c:
To solve the “problem” in part “b” you may want to use the built in
Racket apply, and min/max.
Write a function min&max_apply that does exactly what you did in
part “b” but using apply function.
For example, written in a form of a test that you can use:
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
|#
;; Answer:1.c:

(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (list (apply min (open-list lst)) (apply max (open-list lst))))





#|
Question:2:
In this question we will implement a simple Table data structure. In this data
structure you will need to define a new type called Table. Each element in
the table will be keyed (indexed) with a symbol. In the following the operations
that you are required to implement are detailed below, together with some
guidance.

2.1. Implement the empty table EmptyTbl - this should be a variant of the
data type (constructor).

2.2. Implement the add operation Add - this too should be a variant of the
data type. The add operation should take as input a symbol (key), a string
(value), and an existing table and return an extended table in the natural
way - see examples below.

2.3. Implement the search operation search-table - the search operation
should take as input a symbol (key) and a table and return the first (LIFO,
last in first out) value that is keyed accordingly - see examples below. If
the key does not appear in the original table, it should return a #f value
(make sure the returned type of the function supports this; use the
strictest type possible for the returned type).

2.4. Implement the remove item operation remove-item - the remove item
operation should take as input a table and a symbol (key) and return a
new table contains the items of the original table except of the item
to be deleted without the (first (LIFO) keyed value) - see examples
below. If the original table was empty, it should return an empty table
value.

For example, written in a form of a test that you can use:

(test (EmptyTbl) => (EmptyTbl)) 
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => "AAA")
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl)))) 'a)=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b)=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))

|#





(define-type MyList
    [Empty]
    [Num-node Number MyList]
    [Sym-node Symbol MyList]
    [L-node MyList MyList])


(: countSyms : MyList -> Natural)
(define (countSyms L)

(cases L
    [(Empty) 0]
    [(Sym-node _ rest) (add1 (countSyms rest))]
    [(Num-node _ rest) (countSyms rest)]
    [(L-node lst1 lst2) (+ (countSyms lst1) (countSyms lst2))]))

(countSyms (Sym-node 'w (Sym-node 'q (Num-node 7 (Sym-node 'p (L-node (Empty) (Empty)))))))
(countSyms (Sym-node 'w (Sym-node 'q (Num-node 7 (Sym-node 'p (L-node (Sym-node 'o (Empty)) (Empty)))))))​








