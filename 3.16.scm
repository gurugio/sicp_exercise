

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; test normal data

;; 3
(count-pairs '((a) b))
(count-pairs '(a b c))
(count-pairs '(((a))))
(count-pairs '((a b)))
(count-pairs '(a (b)))
(count-pairs '(() (a)))


;;; test shared data
(define z '(a))

;; 4
(count-pairs (cons z (cons z '())))

;; 7
(count-pairs (cons (cons z z) (cons z z)))

