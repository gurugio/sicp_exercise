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
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


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

(define (weight-ex pair)
  (+ (car pair) (cadr pair)))

(define test-pairs (cons-stream (list (stream-car integers) (stream-car integers))
                                (stream-map (lambda (x) (list x x))
                                            (stream-cdr integers))))
(define test-pairs-1 (stream-filter (lambda (x) (even? (car x)))
                                    test-pairs))
(define test-pairs-2 (stream-filter (lambda (x) (odd? (car x)))
                                    test-pairs))

(display-stream test-pairs-1 10)
(display-stream test-pairs-2 10)

(display-stream (merge-weighted weight-ex test-pairs-1 test-pairs-2) 10)

;; guile> (2 2)
;; (4 4)
;; (6 6)
;; (8 8)
;; (10 10)
;; (12 12)
;; (14 14)
;; (16 16)
;; (18 18)
;; (20 20)
;; (22 22)
;; done
;; guile> guile> (1 1)
;; (3 3)
;; (5 5)
;; (7 7)
;; (9 9)
;; (11 11)
;; (13 13)
;; (15 15)
;; (17 17)
;; (19 19)
;; (21 21)
;; done
;; guile> (1 1)
;; (2 2)
;; (3 3)
;; (4 4)
;; (5 5)
;; (6 6)
;; (7 7)
;; (8 8)
;; (9 9)
;; (10 10)
;; (11 11)
;; done

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; 3.70.a

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))
   


(display-stream (weighted-pairs weight-ex integers integers) 10)
;; guile> (1 1) -2
;; (1 2) -3
;; (2 2) -4
;; (1 3)
;; (2 3) -5
;; (1 4)
;; (3 3) -6
;; (1 5)
;; (2 4)
;; (1 6) -7
;; (3 4)
;; done



;; 3.70.b
(define (weight-ex-b pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define (filter-ex-b pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (cond ((or (= (modulo p1 2) 0) (= (modulo p2 2) 0)) #f)
          ((or (= (modulo p1 3) 0) (= (modulo p2 3) 0)) #f)
          ((or (= (modulo p1 5) 0) (= (modulo p2 5) 0)) #f)
          (else #t))))

(display-stream (stream-filter
                 filter-ex-b
                 (weighted-pairs weight-ex-b integers integers)) 100)
;; guile> (1 1)
;; (1 7)
;; (1 11)
;; (1 13)
;; (1 17)
;; (1 19)
;; (1 23)
;; (1 29)
;; (1 31)
;; (1 37)
;; (1 41)
;; (1 43)
;; (1 47)
;; (1 49)
;; (1 53)
;; (1 59)
;; (1 61)
;; (7 7)
;; (1 67)
;; (1 71)
;; (1 73)
;; (1 77)
;; (1 79)
;; (1 83)
;; (1 89)
;; (1 91)
;; (1 97)
;; (1 101)
;; (1 103)
;; (1 107)
;; (1 109)
;; (1 113)
;; (1 119)
;; (1 121)
;; (1 127)
;; (1 131)
;; (1 133)
;; (1 137)
;; (1 139)
;; (1 143)
;; (1 149)
;; (1 151)
;; (1 157)
;; (1 161)
;; (1 163)
;; (1 167)
;; (1 169)
;; (1 173)
;; (1 179)
;; (1 181)
;; (1 187)
;; (1 191)
;; (1 193)
;; (1 197)
;; (1 199)
;; (1 203)
;; (1 209)
;; (1 211)
;; (1 217)
;; (1 221)
;; (1 223)
;; (1 227)
;; (1 229)
;; (1 233)
;; (1 239)
;; (1 241)
;; (1 247)
;; (1 251)
;; (1 253)
;; (1 257)
;; (1 259)
;; (1 263)
;; (1 269)
;; (1 271)
;; (1 277)
;; (1 281)
;; (1 283)
;; (1 287)
;; (7 11)
;; (1 289)
;; (1 293)
;; (1 299)
;; (1 301)
;; (1 307)
;; (1 311)
;; (1 313)
;; (1 317)
;; (1 319)
;; (1 323)
;; (1 329)
;; (1 331)
;; (1 337)
;; (1 341)
;; (1 343)
;; (1 347)
;; (1 349)
;; (1 353)
;; (1 359)
;; (1 361)
;; (1 367)
;; (1 371)
;; done












