


;; I made parallel-execute for myself and run ex3.43.
;; The result is not like what it said in ex3.43.


(define (parallel-execute . procs)
  (define (body proc-list)
    (if (null? proc-list) 'All-threads-start
        (begin
          (make-thread (car proc-list))
          (body (cdr proc-list)))))
  (body procs))



(define acc1 10)
(define acc2 30)
(define acc3 50)

(define (change12)
  (let ((differ (- acc1 acc2)))
    (set! acc1 (- acc1 differ))
    (set! acc2 (- acc2 differ)))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (change12))
(define (change21)
  (let ((differ (- acc2 acc1)))
    (set! acc2 (- acc2 differ))
    (set! acc1 (- acc1 differ)))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (change12))

(define (change13)
  (let ((differ (- acc1 acc3)))
    (set! acc1 (- acc1 differ))
    (set! acc3 (- acc3 differ)))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (change13))
(define (change31)
  (let ((differ (- acc3 acc1)))
    (set! acc3 (- acc3 differ))
    (set! acc1 (- acc1 differ)))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (change13))

(parallel-execute change12 change13 change21 change31)
;; guile> 30-50
;; 50-50
;; 50-70
;; 70-50
;; 50-90
;; 90-30
;; 30-130
;; 130--30
;; -30-230
;; 230--190
;; -190-490
;; 490--610
;; -610-1170
;; 1170--1710
;; -1710-2950
;; 2950--4590
;; -4590-7610
;; 7610--12130
;; -12130-19810
;; 19810--31870
