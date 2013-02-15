
(define (smallest-divisor n)
  (find-divisor n 2))

;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n ) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (else (find-divisor n (+ test-divisor 1)))))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))


(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (next divisor)
  (if (= divisor 2) 3
      (+ divisor 2)))


(define (runtime) (cdr (gettimeofday)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))


;; result of old smallest-divisor
;; (timed-prime-test 1009)
;; (timed-prime-test 10007)
;; (timed-prime-test 100003)
;; (timed-prime-test 1000003)
;; guile> 
;; 1009 *** 121
;; guile> 
;; 10007 *** 304
;; guile> 
;; 100003 *** 1221
;; guile> 
;; 1000003 *** 9666

;; result of new smallest-divisor -> 1.5-times faster than old result!
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
