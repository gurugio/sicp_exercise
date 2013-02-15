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

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))



;; 3.70

(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           ;; weight값이 같다고 merge프로시저와 같이 하나의 pair 만 merge하면 안됨 
           ;; weight값이 같으면 s1을 먼저 연결할 것
           (cond ((<= (weight s1car) (weight s2car)) ;; must consider same case
                  (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
                 (else
                  (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2)))))))))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))
   
         

;; 3.72

(define (weight-ex pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (+ (* p1 p1) (* p2 p2))))


(define (display-stream-with-weight strm count weight)
  (define (body strm n)
    (if (< n 0) 'done
        (begin (display (stream-car strm)) (display "->") (display (weight (stream-car strm))) (newline)
               (body (stream-cdr strm) (- n 1)))))
  (body strm count))


(display-stream-with-weight (weighted-pairs weight-ex integers integers) 70 weight-ex)


(define (extract pair-stream weight count)
  (if (= count 0) 'done
      (let ((p1 (stream-car pair-stream))
            (p2 (stream-car (stream-cdr pair-stream)))
            (p3 (stream-car (stream-cdr (stream-cdr pair-stream)))))
        (if (= (weight p1) (weight p2) (weight p3))
            (begin (display p1) (display ",") (display p2) (display ",")
                   (display p3) (display "-->") (display (weight p1)) (newline)
                   (extract (stream-cdr pair-stream) weight (- count 1)))
            (extract (stream-cdr pair-stream) weight count)))))

(extract (weighted-pairs weight-ex integers integers)
                   weight-ex
                   6)
;; guile> (1 18),(6 17),(10 15)-->325
;; (5 20),(8 19),(13 16)-->425
;; (5 25),(11 23),(17 19)-->650
;; (7 26),(10 25),(14 23)-->725
;; (2 29),(13 26),(19 22)-->845
;; (3 29),(11 27),(15 25)-->850
;; done
;; guile> 
