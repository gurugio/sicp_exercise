

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))


(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (car (cdr segment)))
(define (make-segment start-point end-point)
  (list start-point end-point))
(define (length-segment segment)
  (let ((a (- (x-point (start-segment segment))
              (x-point (end-segment segment))))
        (b (- (y-point (start-segment segment))
              (y-point (end-segment segment)))))
    (+ (* a a) (* b b))))

; make rectangle with two points
(define (make-rectangle left-top-point right-bottom-point)
  (list left-top-point right-bottom-point))

; select vertical segment and horizontal segment
(define (left-point-rectangle rectangle)
  (if (< (x-point (start-segment rectangle))
         (x-point (end-segment rectangle)))
      (start-segment rectangle)
      (end-segment rectangle)))
(define (right-point-rectangle rectangle)
  (if (< (x-point (start-segment rectangle))
         (x-point (end-segment rectangle)))
      (end-segment rectangle)
      (start-segment rectangle)))
(define (bottom-point-rectangle rectangle)
  (if (< (y-point (start-segment rectangle))
         (y-point (end-segment rectangle)))
      (start-segment rectangle)
      (end-segment rectangle)))
(define (top-point-rectangle rectangle)
  (if (< (y-point (start-segment rectangle))
         (y-point (end-segment rectangle)))
      (end-segment rectangle)
      (start-segment rectangle)))

; select high and width
(define (high-rectangle rectangle)
  (- (y-point (top-point-rectangle rectangle))
     (y-point (bottom-point-rectangle rectangle))))
(define (width-rectangle rectangle)
  (- (x-point (top-point-rectangle rectangle))
     (x-point (bottom-point-rectangle rectangle))))

; compute the area and perimeter
(define (area-rectangle rectangle)
  (* (high-rectangle rectangle)
     (width-rectangle rectangle)))
(define (perimeter-rectangle rectangle)
  (+ (* 2 (high-rectangle rectangle))
     (* 2 (width-rectangle rectangle))))
