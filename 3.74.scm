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


(define (display-stream strm count)
  (define (body strm n)
    (if (< n 0) 'done
        (begin (display (stream-car strm)) (newline)
               (body (stream-cdr strm) (- n 1)))))
  (body strm count))


(define (sign-change-detector cur-val last-val)
  (cond ((and (< last-val 0) (>= cur-val 0)) 1)
        ((and (>= last-val 0) (< cur-val 0)) -1)
        (else 0)))


(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(define sense-data
  (cons-stream 1 (cons-stream 2 (cons-stream 1.5 (cons-stream 1 (cons-stream 0.5 (cons-stream -0.1 (cons-stream -2 (cons-stream -3 (cons-stream -2 (cons-stream -0.5 (cons-stream 0.2 (cons-stream 3 (cons-stream 4 sense-data))))))))))))))

(display-stream sense-data 10)
(display-stream zero-crossings 10)


