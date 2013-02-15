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


(define (display-stream strm)
  (define (body strm n)
    (if (< n 0) 'done
        (begin (display (stream-car strm)) (newline)
               (body (stream-cdr strm) (- n 1)))))
  (body strm 10))

;; 3.65

(define (partial-sums strm)
  (cons-stream (stream-car strm) (add-streams (stream-cdr strm) (partial-sums strm))))

(define (natural-log-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (natural-log-summands (+ n 1)))))

(define natural-log-stream
  (scale-stream (partial-sums (natural-log-summands 1)) 1))

(display-stream natural-log-stream)
;; guile> 1.0
;; 0.5
;; 0.833333333333333
;; 0.583333333333333
;; 0.783333333333333
;; 0.616666666666667
;; 0.759523809523809
;; 0.634523809523809
;; 0.745634920634921
;; 0.645634920634921
;; 0.736544011544012
;; done

(define (square a) (* a a))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-stream (euler-transform natural-log-stream))
;; guile> 0.7
;; 0.690476190476191
;; 0.694444444444444
;; 0.692424242424242
;; 0.693589743589744
;; 0.692857142857143
;; 0.693347338935574
;; 0.693003341687552
;; 0.693253968253968
;; 0.693065750674446
;; 0.693210678210678
;; done

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform
                                      natural-log-stream))
;; guile> 1.0
;; 0.7
;; 0.69327731092437
;; 0.693148869332925
;; 0.693147196073549
;; 0.693147180663564
;; 0.693147180560404
;; 0.693147180559945
;; 0.693147180559943
;; 0.693147180559945
;; +nan.0
;; done
