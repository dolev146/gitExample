#lang pl
#|======== Question 1 ===========
Function: min&max
Description: Finds the minimum and maximum number between 5 numbers.
Consists of an auxiliary function for finding the minimum and an auxiliary function for finding the maximum.
Input: 5 Numbers
Return: Listof Number
Process: To solve, I used lectures and presentations from the exercises, solved after an hour.
================================|#

(: min&max : Number Number Number Number Number -> (Listof Number))
(define (min&max one two three four five)
  (list (find_min one two three four five)(find_max one two three four five))) 

;; Auxiliary function - finding a minimum number
(: find_min : Number Number Number Number Number -> Number)
(define (find_min one two three four five)
  ;; Checks for each number whether it is smaller than the rest
  (cond
  [(and (<= one two)(<= one three)(<= one four)(<= one five)) one]
  [(and (<= two one)(<= two three)(<= two four)(<= two five)) two]
  [(and (<= three one)(<= three two)(<= three four)(<= three five)) three]
  [(and (<= four one)(<= four two)(<= four three)(<= four five)) four]
  [else five]))

;; Auxiliary function - finding a maximum number
(: find_max : Number Number Number Number Number -> Number)
(define (find_max one two three four five)
  ;; Checks for each number whether it is larger than the rest
  (cond
  [(and (>= one two)(>= one three)(>= one four)(>= one five)) one]
  [(and (>= two one)(>= two three)(>= two four)(>= two five)) two]
  [(and (>= three one)(>= three two)(>= three four)(>= three five)) three]
  [(and (>= four one)(>= four two)(>= four three)(>= four five)) four]
  [else five]))

#|======== Question 2.a ===========
Function: sublist-numbers
Description: Extracts all the numbers from a list that contains each type and creates a new list of numbers only.
An auxiliary function is used for tail recursion - helper.
Input: Listof Any
Return: Listof Number
Process: The difficulty was mainly in knowing the syntax of tail recursion,
I solved using an auxiliary function I learned through the presentations of the practice,
the solution time several hours during one day.
================================|#

;; Auxiliary function for creating a list of numbers
(: helper : (Listof Any)(Listof Number) -> (Listof Number))
(define (helper old_list new_list)
  (cond
    [(null? old_list) new_list] ; Go through the entire original list / empty list -> return the newly created list.
    [(number?(first old_list)) ; If an organ in the original list has a number -> then add it to the new list
             (helper (rest old_list)(cons (first old_list) new_list))]
    [else (helper (rest old_list) new_list)]))

; Main function
(: sublist-numbers : (Listof Any) -> (Listof Number))
(define (sublist-numbers list_any)
    (helper list_any '()))

#|======== Question 2.b ===========
Function: min&max-lists
Description: Creates a list of lists that contains lists of pairs of minimum and maximum numbers
and in addition also empty lists from a list of lists of all types.
Uses an auxiliary function - helper2
Input: Listof(Listof Any)
Return: Listof(Listof Number)
Process: It took me a while to understand the work with the list of lists,
I disassembled into parts and it helped me to understand better,
what helped me is the internet mostly, it took me a day and a bit.
================================|#

;; Auxiliary function
(: helper2 : (Listof (Listof Any))(Listof (Listof Number)) -> (Listof (Listof Number)))
(define (helper2 list_of_lists result)
  (cond
    [(null? list_of_lists) (append result '(()))] ; If the list is empty / we have gone through the whole list -> Return the result
    [(null? (sublist-numbers(first list_of_lists)))(helper2 (rest list_of_lists) result)] ; Check using the 2.a function if there is no number in the list -> if there is no go to the next list
    ;If there is at least one number in the list -> Add a new list that contains the minimum and maximum number from the original list.
    [else (helper2 (rest list_of_lists)(append result (list(list(apply min (sublist-numbers(first list_of_lists)))(apply max (sublist-numbers(first list_of_lists))))) ))]))

(: min&max-lists : (Listof(Listof Any)) -> (Listof (Listof Number)))
(define (min&max-lists list_of_lists)
  (helper2 list_of_lists '()))

#|======== Question 3 ===========
define-type: KeyStack
Description: A type that describes a data structure of a stack - Each member consists of a key and a value,
which contains methods of: inserting, removing and searching for an organ.
Process: The difficulty is to get to know for the first time a structure that defines a new type,
I used the last practice and lesson 3 of the lecturer, it took me 5 hours to solve.
================================|#

(define-type KeyStack
  [EmptyKS] ; Empty constructor
  [Push Symbol String KeyStack] ; Constructor containing 3 types
  )

#|================================
Function: search-stack
Description: Looking for an organ inside the stack - ֿ
if it appears several times, returns the first instance.
Input: Symbol, KeyStack
Return: String / Boolean
================================|#
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack key stack)
  (cases stack
    [(EmptyKS) #f] ; Empty stack -> return false
    [(Push k v ks) (cond ; If stack is push constructor type.
                     [(eq? k key) v] ; If the organ is found -> return it.
                     [else (search-stack key ks)])] ; Otherwise go to the next organ.
  ))

#|================================
Function: pop-stack
Description: Removes the last organ that enters.
Input: KeyStack
Return: KeyStack / Boolean
================================|#
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack keyed-stack)
  (cases keyed-stack 
    [(Push k v ks) ks] ; If stack is push constructor type -> return the rest of stack.
    [else #f] ; Empty stack / diffent type -> return false.
  )
 )

;;======================================================= Tests =======================================================

;;---------- Tests - Question 1 ---------------

(test (min&max 2 3 2 7 5) => '(2 7))
(test (min&max 2 1 2 7 5) => '(1 7))
(test (min&max 4 4 -2 4 4) => '(-2 4))
(test (min&max 1 1 1 1 1) => '(1 1))
(test (min&max 1 20 1 0 6) => '(0 20))
(test (min&max 1 1 2 3 4) => '(1 4))
(test (min&max 0 1 5 3 5) => '(0 5))
(test (min&max -5 -4 -3 -2 -1) => '(-5 -1))
(test (min&max -2 -2 -2 -2 -2) => '(-2 -2))
(test (min&max 1000000000000000000000000000000000 5 4 2 1) => '(1 1000000000000000000000000000000000))

;;---------- Tests - Question 2.a ---------------

(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(8 10))
(test (sublist-numbers (list 2 3 4 5)) => '(5 4 3 2))
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '("Hi" "Lior" 'How 'Are You)) => null)
(test (sublist-numbers '(())) => null)
(test (sublist-numbers '((1 2 3 4 5) 6 7 8)) => '(8 7 6))

;;---------- Tests - Question 2.b ---------------

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(('a 'b 'c) (1 2 3))) => '((1 3) ()))
(test (min&max-lists '((-1 -2 -3) (1 2 3))) => '((-3 -1)(1 3) ()))
(test (min&max-lists '(('a r "i" ('e 'l)) ((2 0 2 2)))) => '(()))
(test (min&max-lists '(())) => '(()))

;;---------- Tests - Question 3 ---------------

(test (EmptyKS) => (EmptyKS))
(test (Push 'a "A" (EmptyKS)) => (Push 'a "A" (EmptyKS)))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'c "C" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'c "C" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'b (Push 'a "A" (Push 'b "BB" (Push 'c "CCC" (EmptyKS))))) => "BB")
(test (search-stack 'c (Push 'a "A" (Push 'b "BB" (Push 'c "CCC" (EmptyKS))))) => "CCC")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "A" (EmptyKS)))
(test (pop-stack (Push 'a "A" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)



;;-------------------------------------------------------------------------


(test (min&max 5 4 3 2 1) => '(1 5))
(test (min&max 5 6 3 -5 -9) => '(-9 6))
(test (min&max 0 0 0 0 0) => '(0 0))
(test (min&max -5 -2 -7 -3 -3) => '(-7 -2))
(test (or (equal? (min&max -5 -2.5 -7 -3 -3) '(-7 -2.5)) (equal? (min&max -5 -2.5 -7 -3 -3) '(-7.0 -2.5))) => #t) 
(test (or (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8)) (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8.0))) => #t)
(test (or (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(0 20)) (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(20 0))) => #t) 
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '()) => null)
(test (or (equal? (sublist-numbers '(1 2 3 4 5 6)) '(6 5 4 3 2 1)) (equal? (sublist-numbers '(1 2 3 4 5 6)) '(1 2 3 4 5 6))) => #t) 
(test (or (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(1.5 8 3.3 1 0.2)) (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(0.2 1 3.3 8 1.5))) => #t) 
(test (sublist-numbers '(4 (fd)'((1 2 3)))) => '(4))
(test (sublist-numbers '(any "Benny" OP (2 3))) => '())
(test (or (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(2 3)) (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(3 2))) => #t)
(test (min&max-lists '((2 5 1 5 L) () (4 5 6 7.8 3 2 1) ()))  => '((1 5) () (1 7.8) ()))
(test (min&max-lists '((1) ()))  => '((1 1) ()))
(test (min&max-lists '(("fFAGG" 9 'DEF 2.5) (1.9 "Fd" 4 4.1 4/3) ( 0 "Fd" 0 -1 2/3)))  => '((2.5 9) (4/3 4.1) (-1 2/3)))
(test (min&max-lists '(("fFAGG" 'DEF 3.11) (0)))  => '((3.11 3.11) (0 0)))
(test (min&max-lists '()) => '())
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(() () ())) => '(() () ()))
(test (min&max-lists '((2))) => '((2 2)))
(test (min&max-lists '((2) ("test" (1 2)))) => '((2 2) ()))
(test (min&max-lists '((2 2) ('q) (-1 -1 1 1))) => '((2 2) () (-1 1)))
(test (min&max-lists '(() ("test") ('a))) => '(() () ()))
(test (min&max-lists '((E S S A L) (a A Z E R T 1) ())) => '(() (1 1) ()))
(test (EmptyKS) => (EmptyKS))
(test (Push 'a "a" (Push 'A "A" (EmptyKS))) => (Push 'a "a" (Push 'A "A" (EmptyKS))) )
(test (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))) =>(Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))))
(test (search-stack 'aaa (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => "AA")
(test (search-stack 'f (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => "f")
(test (search-stack 'g (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => #f)
(test (search-stack 'E (Push 'EEE "EEE" (Push 'V "V" (Push 'Q "QQ" (EmptyKS))))) => #f)
(test (search-stack 'E (EmptyKS)) => #f)
(test (pop-stack (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))
#|(test (pop-stack (Push 'A "A" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (pop-stack (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS)))))) => (Push 'c "C" (EmptyKS)))
(test (pop-stack (pop-stack (pop-stack (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS))))))) => (EmptyKS))
(test (pop-stack (pop-stack (pop-stack (pop-stack (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS)))))))) => #f)
(test (search-stack 'a (Push 'aaaa "Worng" (Push 'aaa "Worng" (Push 'aa "Worng" (Push 'a "Right!!!" (Push 'a "Worng" (EmptyKS))))))) => "Right!!!")|#
      



