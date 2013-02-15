(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              (display "insert value") (display "-")
              (display x) (display ":") (display result)
              (newline)
              result))))))

(define memo-fib
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (memo-fib (- n 1))
                             (memo-fib (- n 2))))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


;; (memo-fib 10) stores result values of each steps of lambda
(memo-fib 10)
;; result of (memo-fib 10)
;; guile> insert value-1:1
;; insert value-0:0
;; insert value-2:1
;; insert value-3:2
;; insert value-4:3
;; insert value-5:5
;; insert value-6:8
;; insert value-7:13
;; insert value-8:21
;; insert value-9:34
;; insert value-10:55
;; 55


;; store final result value
((memorize fib) 10)
;; result of ((memorize fib) 10)
;; guile> insert value:55
