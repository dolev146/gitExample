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
(countSyms (Sym-node 'w (Sym-node 'q (Num-node 7 (Sym-node 'p (L-node (Sym-node 'o (Empty)) (Empty)))))))