


(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1))) ;; f(f(f(...)))
      f)) ;; (f(f(f(...f(x))))) the last f

((repeated square 2) 5)
