

;; primitive procedures
(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (square x)
  (* x x))

(define tolerance 0.00001)


;; iterative-improve
(define (iterative-improve is-good? get-next-guess)
  (define (iterative-improve-body guess)
    (if (is-good? guess)
        guess
        (iterative-improve-body (get-next-guess guess))))
  iterative-improve-body)


;; results

(define (sqrt-ex x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point-ex f first-guess)
  (define (close-enough? v1)
    (< (abs (- v1 (f v1))) tolerance))
  ((iterative-improve close-enough? f) first-guess))


;; test
(sqrt-ex 2) ;; 1.414..
(fixed-point-ex cos 1.0) ;; 0.739..
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0) ;; golden ratio 1.618..
