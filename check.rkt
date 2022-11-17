
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

(: min&max_apply (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply l)
  (apply list (apply min (map car l)) (apply max (map car l))))






