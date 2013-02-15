


(define (equal-ex? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((and (pair? (car list1)) (pair? (car list2)))
         (and (equal-ex? (car list1) (car list2))
              (equal-ex? (cdr list1) (cdr list2))))
        ((not (eq? (car list1) (car list2))) #f)
        (else (equal-ex? (cdr list1) (cdr list2)))))

(equal-ex? '(this is a list1) '(this is a list2))
(equal-ex? '(this is a list1) '(this is a list1))
(equal-ex? '(this is a list1) '(this (is a) list1))
(equal-ex? '(this (is a) list1) '(this (is the) list1))
(equal-ex? '(this is a list1) '(this (is a) list1))
(equal-ex? '(this (is a) list1) '(this (is a) list1))
(equal-ex? '(this (is (a or the)) list1) '(this (is (a or the)) list1))
(equal-ex? '(this (is (a or the)) list1) '(this (is (an or the)) list1))
