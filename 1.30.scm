(define (cube x)
  (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n)) ;; constant
  (define (y n)
    (f (+ a (* n h))))

;; next a
;; When n is 100, a is 1/100, 2/100, 3/100 ......
  (define (next x)
    (+ x h))

;; value of each term
;; When n is 100, 4*f(a1) + 2*f(a2) + 4*f(a3) ......
  (define (term k)
    (if (even? (* k n))
        (* 2.0 (cube k))
        (* 4.0 (cube k))))

  (* (/ h 3)
     (+ (sum-iter term (+ a h) next (- b h))
        (y 0)
        (y n)))
)


;; (define (sum term a next b)
;;   (if (> a b) 0
;;       (+ (term a) (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        ;; a <- (next a)
        ;; result <- old-result + new-result = result + (term a)
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; test

(simpson cube 0 1 100) ; 0.25
(simpson cube 0 1 400) ; 0.25
;; guile cannot execute recursive procedure 1000-times.
;; Almost 400 might be maximum


