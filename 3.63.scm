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


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))




;;3.63

(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (display "new guess=") (display (average guess (/ x guess))) (newline)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess) (sqrt-improve guess x))
                           (sqrt-stream x))))


(define (sqrt-stream-fast x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define slow (sqrt-stream 4))
(stream-ref slow 0)
(stream-ref slow 1)
(stream-ref slow 2)
(stream-ref slow 3)
(stream-ref slow 4)
;; new guess=2.5
;; new guess=2.5
;; new guess=2.05
;; new guess=2.5
;; new guess=2.05
;; new guess=2.00060975609756
;; new guess=2.5
;; new guess=2.05
;; new guess=2.00060975609756
;; new guess=2.00000009292229
;; 2.00000009292229 
;; -> (sqrt-stream x) is repeated!

(define fast (sqrt-stream-fast 4))
(stream-ref fast 0)
(stream-ref fast 1)
(stream-ref fast 2)
(stream-ref fast 3)
(stream-ref fast 4)
;; guile> guile> new guess=2.5
;; new guess=2.05
;; new guess=2.00060975609756
;; new guess=2.00000009292229
;; 2.00000009292229
;; guile> 
