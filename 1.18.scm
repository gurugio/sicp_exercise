
(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul-recur a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mul-recur a (halve b))))
        (else (+ a (fast-mul-recur a (- b 1))))))

(define (fast-mul-iter a b)
  (fast-mul-iter-body a b 0))

(define (fast-mul-iter-body a b product)
  (cond ((= b 1) (+ a product))
        ((even? b) (fast-mul-iter-body (double a) (halve b) product))
        (else (fast-mul-iter-body a (- b 1) (+ product a)))))

(define fast-mul fast-mul-iter)

(fast-mul 3 4)
(fast-mul 4 8)
(fast-mul 5 16)

(fast-mul 3 5)
(fast-mul 4 6)
(fast-mul 3 7)
(fast-mul 3 8)
(fast-mul 3 9)
(fast-mul 3 10)
(fast-mul 3 11)
(fast-mul 3 12)

