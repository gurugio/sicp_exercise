

(define (square x) (* x x))

(define (tree-map procedure tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) (tree-map procedure sub-tree)
             (procedure sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree '(1 (2 (3 4) 5) (6 7)))
