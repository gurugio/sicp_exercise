
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

;; O(n/2)?
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(adjoin-set 1 '(2 4 6 8))
(adjoin-set 3 '(2 4 6 8))
(adjoin-set 5 '(2 4 6 8))
(adjoin-set 7 '(2 4 6 8))
(adjoin-set 9 '(2 4 6 8))
(adjoin-set 4 '(2 4 6 8))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(intersection-set '(1 3 5 7 9) '(2 5 7 8))

