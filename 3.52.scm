;;---------- prime? -------------------------------
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
;;------------------------------------------------

;; for Guile implementation
(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


;; user native force of Guile
;; (define (force delayed-object)
;;   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-map proc . argstreams)
  (if (stream-null? (stream-car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
  
  
;;ex3.52

(define sum 0)
;; sum = 0


(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
;; process one item (=1), therefore sum will be 1.
;; guile> guile> sum
;; 1
;; guile> seq
;; (1 . #<promise #<procedure #f ()>>)


(define y (stream-filter even? seq))
;; process until even number 6.
;; guile> seq
;; (1 . #<promise (3 . #<promise (6 . #<promise #<procedure #f ()>>)>)>)
;; guile> sum
;; 6
;; guile> 

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;; process until meet 10
;; guile> sum
;; 10
;; guile> seq
;; (1 . #<promise (3 . #<promise (6 . #<promise (10 . #<promise #<procedure #f ()>>)>)>)>)
;; guile> 

(stream-ref y 7) ;; 136
(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210done


;; final status
;; guile> sum
;; 210
;; guile> seq
;; (1 . #<promise (3 . #<promise (6 . #<promise (10 . #<promise (15 . #<promise (21 . #<promise (28 . #<promise (36 . #<promise (45 . #<promise (55 . #<promise (66 . #<promise (78 . #<promise (91 . #<promise (105 . #<promise (120 . #<promise (136 . #<promise (153 . #<promise (171 . #<promise (190 . #<promise (210 . #<promise ()>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)>)


;; If delay is normal lambda procedure, seq is initialized with full entry.



