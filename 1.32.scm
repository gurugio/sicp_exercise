;; (define (product term a next b)
;;   (if (> a b) 1
;;       (* (term a) (product term (next a) next b))))

;; (define (product-iter term a next b)
;;   (define (product-iter-body a result)
;;     (if (> a b) result
;;         (product-iter-body (next a) (* result (term a)))))
;;   (product-iter-body a 1))

;; (define (sum term a next b)
;;   (if (> a b) 0
;;       (+ (term a) (sum term (next a) next b))))

;; (define (sum-iter term a next b)
;;   (define (iter a result)
;;     (if (> a b)
;;         result
;;         ;; a <- (next a)
;;         ;; result <- old-result + new-result = result + (term a)
;;         (iter (next a) (+ result (term a)))))
;;   (iter a 0))
  

;; extract the common potion of sum and product

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (term-for-pi n)
  (if (< n 2) 0
      (/ (* (- n 1) (+ n 1)) (* n n))))
(define (next-for-pi n)
  (+ n 2))

;; PI = 4 * product of 3 ~ 99
(* 4.0 (product term-for-pi 3 next-for-pi 99))
(* 4.0 (product-iter term-for-pi 3 next-for-pi 99))

(sum + 1 (lambda (x) (+ x 1)) 10)
(sum-iter + 1 (lambda (x) (+ x 1)) 10)
