
;;(define (runtime) (tms:clock (times)))
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

;========================================================
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))
;===========================================================


(define (find-prime from count)
  (if (> count 0)
      (if (prime? from)
          (begin (display from) (newline)
                 (find-prime (+ from 1) (- count 1)))
          (find-prime (+ from 1) count))))

(find-prime 1000 3)
(find-prime 10000 3)
(find-prime 100000 3)
(find-prime 1000000 3)
;; guile> 1009
;; 1013
;; 1019
;; guile> 10007
;; 10009
;; 10037
;; guile> 100003
;; 100019
;; 100043
;; guile> 1000003
;; 1000033
;; 1000037
;; guile> 

(timed-prime-test 1009)
(timed-prime-test 10007)
(timed-prime-test 100003)
(timed-prime-test 1000003)
;; guile> 
;; 1009 *** 121
;; guile> 
;; 10007 *** 304
;; guile> 
;; 100003 *** 1221
;; guile> 
;; 1000003 *** 9666
;; guile> 

;; (sqrt 10) = 3.16..
;; 304 := 121*3
;; 1221 := 304 *3
;; 9666 := 1221 *3

;; The consumed-time is increased by (sqrt 10)-times
