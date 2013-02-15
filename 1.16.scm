
(define (square a) (* a a))

(define (fast-expt-recur b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recur b (/ n 2))))
        (else (* b (fast-expt-recur b (- n 1))))))

(define (fast-expt-iter b n)
  (fast-expt-iter-body b n 1))


;; 2^11 = 2*2^10  -> b=2, count=10, product=2
;; 2^10 = 2^2^5   -> b=2^2, count=5, product=2
;; When even, change b. When odd, change product
;; When count=1, final b and final product are multiplied.
(define (fast-expt-iter-body b count product)
  (cond ((= count 0) product)
        ((even? count) (fast-expt-iter-body (square b) (/ count 2) product))
        (else (fast-expt-iter-body b (- count 1) (* product b)))))

(define (test n)
  (if (= n 1) (display "end")
      (begin (display n)
             (display "->")
             (display (fast-expt-iter 2 n))
             (newline)
             (test (- n 1)))))
(test 11)

(fast-expt-iter 2 7)
