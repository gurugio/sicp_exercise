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




;; 3.59 - a.
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series coeffs)
  (stream-map / coeffs integers))

;; 3.59 - b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (integrate-series
                  (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; 3.60

;; (car s1)*(car s2) -> R1
;; + (car s1)*(cdr s2) -> R2
;; + (cdr s1)*(car s2) -> R3
;; + (cdr s1)*(cdr s1) -> R4 -> constant must be ZERO!
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) ;R1
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1)) ;R2
                (add-streams
                 (scale-stream (stream-cdr s1) (stream-car s2)) ; R3
                 (cons-stream 0
                              (mul-series (stream-cdr s1)
                                          (stream-cdr s2))))))) ;R4

(define re (add-streams (mul-series cosine-series cosine-series)
                        (mul-series sine-series sine-series)))
(stream-ref re 0)
(stream-ref re 1)
(stream-ref re 2)
(stream-ref re 3)
(stream-ref re 4)
