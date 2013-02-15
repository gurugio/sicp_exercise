

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(compose square inc) ;; -> return procedure
((compose square inc) 6)
