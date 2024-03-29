
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define test1-tree '())
(define test1-tree (adjoin-set 7 test1-tree))
(define test1-tree (adjoin-set 3 test1-tree))
(define test1-tree (adjoin-set 9 test1-tree))
(define test1-tree (adjoin-set 1 test1-tree))
(define test1-tree (adjoin-set 5 test1-tree))
(define test1-tree (adjoin-set 11 test1-tree))
test1-tree

(define test2-tree '(3 (1 () ()) 
                       (7 (5 () ()) (9 () (11 () ())))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(tree->list-1 test1-tree)
(tree->list-1 test2-tree)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(tree->list-2 test1-tree)
(tree->list-2 test2-tree)
