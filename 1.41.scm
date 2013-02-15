
(define (inc x)
  (+ x 1))

(define (double proc)
  (lambda (x) (proc (proc x))))

((double inc) 3)

(((double (double double)) inc) 5)



;; (double double)
;; -> (double (double x)) -> 2*2 times

;; (double (double double))
;; -> ((double (double x)) (double (double x')))
;; -> substitute x with (double (double x'))
;; -> ((double (double (double (double x')))))
;; -> (double of 2*2)
;; -> 2*2 * 2*2 -> 16-times
