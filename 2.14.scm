;=========== 2.14 =====================
(define (make-center-percent c p)
  (let ((ratio (* c (/ p 100))))
    (make-interval (- c ratio) (+ c ratio))
    )
  )

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
  )

(define (percent p)
  (* 100 (/ (- (upper-bound p) (center p))))
  )

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y)))
  )

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4)))
  )

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))
  )


(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2)
                   (max p1 p2)))
  )

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(div-interval (make-center-percent 10 0.01) (make-center-percent 10 0.01))
;(9.99 10.01) / (9.99 10.01) 을 계산기로 계산하면 (0.998001 1.002002)로 계산되지만
;실행결과는 (0.9998000199980004 . 1.000200020002) 이다.
;결국 원래는 0.2%의 오차를 가져야 하는 범위 값이 0.02%의 오차를 가진다고 계산되버린다.

