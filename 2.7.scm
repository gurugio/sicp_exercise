(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; My solution is following..
;; (define (sub-interval x y)
;;   (make-interval (- (lower-bound x) (upper-bound y))
;;                  (- (upper-bound x) (lower-bound x))))
;; copy from somewhere
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
