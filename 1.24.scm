
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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


;; result of ex1.23
;; 1009 *** 93
;; 1013 *** 105
;; 1019 *** 75
;; 10007 *** 241
;; 10009 *** 221
;; 10037 *** 222
;; 100003 *** 914
;; 100019 *** 809
;; 100043 *** 793
;; 1000003 *** 10653
;; 1000033 *** 2012
;; 1000037 *** 2196


;; result of ex1.24
;; time-consuming is increasing by O(logN).
;; But consumed-time of small value is bigger than old test procedure.
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
;; 1009 *** 563
;; 1013 *** 562
;; 1019 *** 566
;; 10007 *** 667
;; 10009 *** 730
;; 10037 *** 937
;; 100003 *** 847
;; 100019 *** 8917
;; 100043 *** 747
;; 1000003 *** 833
;; 1000033 *** 834
;; 1000037 *** 871
