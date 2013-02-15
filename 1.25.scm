
(define (expmod base exp m)
  (display base)
  (newline)
  (display exp)
  (newline)
  (remainder (expt base exp) m))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(fast-prime? 1009 10)
(fast-prime? 1008 10)


(define (runtime) (cdr (gettimeofday)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))



(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)


;; For (expt base exp), base and exp is very big value.
;; Therefore the time to calculate (expt base exp) becomes big.
;; result
;; 1009 *** 865
;; guile> 
;; 1013 *** 676
;; guile> 
;; 1019 *** 711
;; guile> 
;; 10007 *** 19754
;; guile> 
;; 10009 *** 15045
;; guile> 
;; 10037 *** 17016
;; guile> 
;; 100003 *** 231337
;; guile> 
;; 100019 *** 224720
;; guile> 
;; 100043 *** 230562
;; guile> 
;; 1000003 *** 213352
