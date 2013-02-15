(define ex1 (list 1 3 (list 5 7) 9))
(define ex2 (list (list 7)))
(define ex3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (find-seven tree)
  (display tree) (newline)
  (cond ((null? tree))
        ((and (number? tree) (equal? tree 7)) (display "found"))
        ((list? tree)
         (find-seven (car tree)) (find-seven (cdr tree)))))

(car (cdr (car (cdr (cdr ex1)))))
(car (car ex2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ex3))))))))))))
