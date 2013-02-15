

(define (mul a b)
  (if (= b 1)
      a
      (+ a (mul a (- b 1)))))

(mul 3 4)
(mul 4 4)

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

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
