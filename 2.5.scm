
; cons
(define (expt-cons a b)
  (* (expt 2 a) (expt 3 b)))

; get 2^a
(define (two-product expt)
  (if (= 0 (remainder expt 3))
      (two-product (/ expt 3))
      expt))
;get 3^b
(define (three-product expt)
  (if (= 0 (remainder expt 2))
      (three-product (/ expt 2))
      expt))

;get a
(define (two-expo-body num count)
  (if (= 1 (/ num 2))
      count
      (two-expo-body (/ num 2) (+ count 1))))
(define (two-expo num)
  (two-expo-body num 1))

;get b
(define (three-expo-body num count)
  (if (= 1 (/ num 3))
      count
      (three-expo-body (/ num 3) (+ count 1))))
(define (three-expo num)
  (three-expo-body num 1))

; car
(define (expt-car expt)
  (two-expo (two-product expt)))
; cdr
(define (expt-cdr expt)
  (three-expo (three-product expt)))
