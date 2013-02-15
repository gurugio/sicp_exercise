;=================== 1.31 =======================

(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (product-iter-body a result)
    (if (> a b) result
        (product-iter-body (next a) (* result (term a)))))
  (product-iter-body a 1))
  


(define (term-for-pi n)
  (if (< n 2) 0
      (/ (* (- n 1) (+ n 1)) (* n n))))
(define (next-for-pi n)
  (+ n 2))

;; PI = 4 * product of 3 ~ 99
(* 4.0 (product term-for-pi 3 next-for-pi 99))


