
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (car set-of-records))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(lookup 11 '(1 2 3 11 12 13))

(define test2-tree '(3 (1 () ()) 
                       (7 (5 () ()) (9 () (11 () ())))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (lookup-btree-1 given-key bin-tree)
  (lookup given-key (tree->list-1 bin-tree)))

(lookup-btree-1 9 test2-tree)

(define (lookup-btree-2 given-key bin-tree)
  (cond ((null? bin-tree) #f)
        ((equal? given-key (entry bin-tree))
         (entry bin-tree))
        (else
         (or (lookup-btree-2 given-key (left-branch bin-tree))
             (lookup-btree-2 given-key (right-branch bin-tree))))))

(lookup-btree-2 3 test2-tree)
(lookup-btree-2 1 test2-tree)
(lookup-btree-2 7 test2-tree)
(lookup-btree-2 5 test2-tree)
(lookup-btree-2 9 test2-tree)
(lookup-btree-2 11 test2-tree)
(lookup-btree-2 13 test2-tree)
