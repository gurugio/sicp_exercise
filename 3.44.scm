


;; I made parallel-execute for myself and run ex3.44.
;; The result is not like what it said in ex3.44

(define (parallel-execute . procs)
  (define (body proc-list)
    (if (null? proc-list) 'All-threads-start
        (begin
          (make-thread (car proc-list))
          (body (cdr proc-list)))))
  (body procs))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (lock-mutex mutex)
        (let ((val (apply p args)))
          (unlock-mutex mutex)
          val))
      serialized-p)))



;; test without serializer
(define acc1 10)
(define acc2 30)
(define acc3 50)

(define amount 10)
(define (transfer12)
  (set! acc1 (- acc1 amount))
  (set! acc2 (+ acc2 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (transfer12))
(define (transfer21)
  (set! acc2 (- acc2 amount))
  (set! acc1 (+ acc1 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (transfer21))
(define (transfer13)
  (set! acc1 (- acc1 amount))
  (set! acc3 (+ acc3 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (transfer13))
(define (transfer31)
  (set! acc3 (- acc3 amount))
  (set! acc1 (+ acc1 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (transfer31))

(parallel-execute transfer12 transfer21 transfer13 transfer31)
;; guile> 10-30
;; 0-40
;; 0-50
;; 20-40
;; 10-30
;; 10-40
;; 0-50
;; 10-40
;; 10-30
;; 10-20
;; 20-50
;; 10-60
;; 10-30
;; 0-50
;; 10-40
;; 20-40
;; 10-30
;; 0-50
;; 10-40
;; 20-40
;; 1010-30-
;; 50
;; 10-40
;; 20-40
;; 10-30
;; 10-50
;; 10-4010
;; -40
;; 10-50
;; 20-30
;; 30-20
;; 20-40
;; 10-50
;; 20-30
;; 30-20


;; test with serializer
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


(define acc1 10)
(define acc2 30)
(define acc3 50)

(define amount 10)
(define (transfer12)
  (set! acc1 (- acc1 amount))
  (set! acc2 (+ acc2 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (transfer12))
(define (transfer21)
  (set! acc2 (- acc2 amount))
  (set! acc1 (+ acc1 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc2) (newline)
  (transfer21))
(define (transfer13)
  (set! acc1 (- acc1 amount))
  (set! acc3 (+ acc3 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (transfer13))
(define (transfer31)
  (set! acc3 (- acc3 amount))
  (set! acc1 (+ acc1 amount))
  (sleep 1)
  (display acc1) (display "-") (display acc3) (newline)
  (transfer31))

(define s (make-serializer))
(parallel-execute (s (lambda () ((transfer12))))
                  (s (lambda () ((transfer21))))
                  (s (lambda () ((transfer13))))
                  (s (lambda () ((transfer31)))))
;; guile> 0-40
;; -10-50
;; -20-60
;; -30-70
;; -40-80
;; -50-90
;; -60-100
;; -70-110
;; -80-120
;; -90-130
;; -100-140
