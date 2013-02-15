


;; a1 = b0*q + a0*q + a0*p
;; b1 = b0*p + a0*q

;; a2 = b1*q + a1*q + a1*p
;; b2 = b1*p + a1*q

;; substitue a1 and b1 in the second equation with the first equation,
;; so that a2 and b2 will be expressed with a0 and b0.
;; a1 = b0*(q^2 + 2*p*q) + a0*(q^2 + 2*p*q) + a0*(q^2 + p^2)
;; b1 = b0*(q^2 + p^2) + a0*(q^2 + 2*p*q)

;; Therefore, p' = p^2 + q^2, q' = q^2 + 2*p*q.


(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* q q) (* 2 p q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
;; guile> 1
;; guile> 1
;; guile> 2
;; guile> 3
;; guile> 5
;; guile> 8
;; guile> 13
;; guile> 21
;; guile> 34

