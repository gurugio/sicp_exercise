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




(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(display-stream (pairs integers integers) 10)

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs (stream-cdr s) (stream-cdr t)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(display-stream (triples integers integers integers) 30)
;; guile> (1 1 1)
;; (1 2 2)
;; (2 2 2)
;; (1 2 3)
;; (2 3 3)
;; (1 3 3)
;; (3 3 3)
;; (1 2 4)
;; (2 3 4)
;; (1 3 4)
;; (3 4 4)
;; (1 2 5)
;; (2 4 4)
;; (1 4 4)
;; (4 4 4)
;; (1 2 6)
;; (2 3 5)
;; (1 3 5)
;; (3 4 5)
;; (1 2 7)
;; (2 4 5)
;; (1 4 5)
;; (4 5 5)
;; (1 2 8)
;; (2 3 6)
;; (1 3 6)
;; (3 5 5)
;; (1 2 9)
;; (2 5 5)
;; (1 5 5)
;; (5 5 5)
;; done


(define (pythagorean-filter triple)
  (let ((i (car triple))
        (j (cadr triple))
        (k (caddr triple)))
    (and (<= i j) (= (* k k) (+ (* i i) (* j j))))))

(pythagorean-filter '(3 4 5))
(pythagorean-filter '(3 4 6))

(display-stream (stream-filter pythagorean-filter (triples integers integers integers)) 5)
;;  (3 4 5)
;; (6 8 10)
;; (5 12 13)
;; (9 12 15)
;; (8 15 17)
;; (12 16 20)
