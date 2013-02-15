(define (make-accumulator num)
  (lambda (addor)
    (begin (set! num (+ num addor))
           num)))

(define A (make-accumulator 5))
(A 10)
(A 10)
