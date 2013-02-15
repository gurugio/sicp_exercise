

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i) (- (* 2 i) 1))
  (define (cont-frac n d k)
    (define (cont-frac-body count)
      (if (> count k)
          0
          (/ (n count) (+ (d count) (cont-frac-body (+ count 1))))))
    (cont-frac-body 1))
  (cont-frac n d k))


(define pi 3.14159265)
 
(tan (/ pi 4))
(tan-cf (/ pi 4) 10)

(tan (* 3 (/ pi 4)))
(tan-cf (* 3 (/ pi 4)) 10)

(tan pi)
(tan-cf pi 10)
