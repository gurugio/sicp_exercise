(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else 
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-with-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) (square-tree-with-map sub-tree)
             (* sub-tree sub-tree)))
       tree))


(square-tree '(1 (2 (3 4) 5) (6 7)))
(square-tree-with-map '(1 (2 (3 4) 5) (6 7)))


