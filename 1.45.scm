




(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1))) ;; f(f(f(...)))
      f)) ;; (f(f(f(...f(x))))) the last f


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
               1.0))

(define (4th-power-root x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (* y y y)))) 2)
               1.0))


(define (nth-power-root x n)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) n)
               1.0))


(nth-power-root 3 2)
(sqrt 3)

(nth-power-root 8 3)
(cube-root 8)

(nth-power-root 16 4)
(4th-power-root 16)

;; result
;; 1.73205080756888
;; 1.73205080756888
;; 1.99999954561909
;; 1.99999818247885
;; 1.98648905288242
;; 1.98298515517235
