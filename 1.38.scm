



(define (cont-frac n d k)
  (define (cont-frac-body count)
    (if (> count k)
        0
        (/ (n count) (+ (d count) (cont-frac-body (+ count 1))))))
  (cont-frac-body 1))

(define (n i) 1.0)
(define (d i)
  (cond ((= (modulo i 3) 0) 1)
        ((= (modulo i 3) 1) 1)
        (else
         (* 2 (/ (+ i 1) 3)))))


(+ (cont-frac n d 10) 2)


