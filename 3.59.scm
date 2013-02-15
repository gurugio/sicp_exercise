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


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;; 3.59 - a.
(define C 0)

(define (integrate-series coeffs)
  (cons-stream C (stream-map / coeffs integers)))

(define test (integrate-series ones))
(stream-ref test 0)
(stream-ref test 1)
(stream-ref test 2)
(stream-ref test 3)
(stream-ref test 4)

(define test2 (integrate-series integers))
(stream-ref test2 0)
(stream-ref test2 1)
(stream-ref test2 2)
(stream-ref test2 3)
(stream-ref test2 4)


;; 3.59 - b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define (negate-series series)
  (stream-map - series))

(define cosine-series
  (cons-stream 1 (negate-series (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)
(stream-ref cosine-series 5)
(stream-ref cosine-series 6)
(stream-ref cosine-series 7)
(stream-ref cosine-series 8)
(stream-ref cosine-series 9)

(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
(stream-ref sine-series 5)
(stream-ref sine-series 6)
(stream-ref sine-series 7)
(stream-ref sine-series 8)
(stream-ref sine-series 9)

