(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; copy from Ken Dyck's Weblog
(define (mul-interval x y)
  (let* ((lx (lower-bound x))
         (ux (upper-bound x))
         (ly (lower-bound y))
         (uy (upper-bound y))
         (pos-lx? (positive? lx))
         (pos-ux? (positive? ux))
         (pos-ly? (positive? ly))
         (pos-uy? (positive? uy)))
    (cond 
      ; lx ux ly uy  example
      ; ----------------------------------
      ;  +  -  +  +  invalid interval
      ;  +  -  +  -  invalid interval
      ;  +  -  -  +  invalid interval
      ;  +  -  -  -  invalid interval
      ((and pos-lx? (not pos-ux?))
       (error "invalid interval" x))

      ;  +  +  +  -  invalid interval
      ;  -  +  +  -  invalid interval
      ;  -  -  +  -  invalid interval
      ((and pos-ly? (not pos-uy?))
       (error "invalid interval" y))
      
      ;  +  +  +  +  (1.2)(2.3) = (2.6) 
      ((and pos-lx? pos-ux? pos-ly? pos-uy?)
       (make-interval (* lx ly) (* ux uy)))
      
      ;  +  +  -  +  (1.2)(-2.3) = (-4.6)
      ((and pos-lx? pos-ux? (not pos-ly?) pos-uy?)
       (make-interval (* ux ly) (* ux uy)))
      
      ;  +  +  -  -  (1.2)(-2.-1) = (-4.-1) 
      ((and pos-lx? pos-ux? (not pos-ly?) (not pos-uy?))
       (make-interval (* ux ly) (* lx uy)))
      
      ;  -  +  +  +  (-1.2)(2.3) = (-3.6)
      ((and (not pos-lx?) pos-ux? pos-ly? pos-uy?)
       (make-interval (* lx uy) (* ux uy)))
      
      ;  -  +  -  +  (-1.2)(-2.3) = (-4.6) *
      ((and (not pos-lx?) pos-ux? (not pos-ly?) pos-uy?)
       (make-interval (min (* lx uy) (* ux ly))
                      (* ux uy)))
      
      ;  -  +  -  -  (-1.2)(-2.-1) = (-4.2)
      ((and (not pos-lx?) pos-ux? (not pos-ly?) (not pos-uy?))
       (make-interval (* ux ly) (* lx ly)))
      
      ;  -  -  +  +  (-2.-1)(2.3) = (-6.-2)
      ((and (not pos-lx?) (not pos-ux?) pos-ly? pos-uy?)
       (make-interval (* lx uy) (* ux ly)))

      ;  -  -  -  +  (-2.-1)(-2.3) = (-6, 4)
      ((and (not pos-lx?) (not pos-ux?) (not pos-ly?) pos-uy?)
       (make-interval (* lx uy) (* lx ly)))

      ;  -  -  -  -  (-2.-1)(-2.-1) = (1.4)
      ((and (not pos-lx?) (not pos-ux?) (not pos-ly?) (not pos-uy?))
       (make-interval (* ux uy) (* lx ly))))))

  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (zero? (upper-bound y)) (zero? (lower-bound y)))
      (display "Error: second interval has zero value\n")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; (define (sub-interval x y)
;;   (make-interval (- (lower-bound x) (upper-bound y))
;;                  (- (upper-bound x) (lower-bound x))))
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (compare-width x y operation)
  (let ((width-x (width-interval x))
        (width-y (width-interval y))
        (width-result (width-interval (operation x y))))
    (cond ((equal? operation add-interval) 
           (display (+ width-x width-y))
           (display "=?")
           (display width-result)
           (newline))
          ((equal? operation sub-interval)
           (display (abs (- width-x width-y)))
           (display "=?")
           (display width-result)
           (newline))
          ((equal? operation mul-interval)
           (display (* width-x width-y))
           (display "=?")
           (display width-result)
           (newline))
          (else (display "fail")))
    ))


; test cases for handling zero point
(define intv1 (make-interval 0 3))
(define intv2 (make-interval -3 0))
(define intv3 (make-interval -3 -2))
(define intv4 (make-interval 2 3))
(define intv5 (make-interval 0 0))

(mul-interval intv1 intv1)
(mul-interval intv1 intv2)
(mul-interval intv1 intv3)
(mul-interval intv1 intv4)
(mul-interval intv2 intv1)
(mul-interval intv2 intv2)
(mul-interval intv2 intv3)
(mul-interval intv2 intv4)
(mul-interval intv3 intv1)
(mul-interval intv3 intv2)
(mul-interval intv3 intv3)
(mul-interval intv3 intv4)
(mul-interval intv4 intv1)
(mul-interval intv4 intv2)
(mul-interval intv4 intv3)
(mul-interval intv4 intv4)
(mul-interval intv5 intv1)
(mul-interval intv5 intv2)
(mul-interval intv5 intv3)
(mul-interval intv5 intv4)


;; Test result:
;; guile> (0 . 9)
;; guile> (-9 . 0)
;; guile> (-9 . 0)
;; guile> (0 . 9)
;; guile> (-9 . 0)
;; guile> (0 . 9)
;; guile> (0 . 9)
;; guile> (-9 . 0)
;; guile> (-9 . 0)
;; guile> (0 . 9)
;; guile> (4 . 9)
;; guile> (-9 . -4)
;; guile> (0 . 9)
;; guile> (-9 . 0)
;; guile> (-9 . -4)
;; guile> (4 . 9)
