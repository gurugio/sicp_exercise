


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1))) ;; f(f(f(...)))
      f)) ;; (f(f(f(...f(x))))) the last f



(define (square x) (* x x))
(define (inc x) (+ x 1))


(define dx 0.001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

((smooth square) 3)

;; return procedure that do smooth of square 4 times
((repeated smooth 4) square) 

;; do smooth of square of 3 by 4-times
(((repeated smooth 4) square) 3)
