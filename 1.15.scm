
(define p-count 0)

(define (cube x) (* x x x))
(define (p x) (begin (set! p-count (+ p-count 1))
                     (- (* 3 x) (* 4 (cube x)))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(define (sine-wrapper angle)
  (set! p-count 0)
  (sine angle)
  (display "p is called ")
  (display p-count)
  (display "-times")
  (newline))


;; (sine 12.15)
;; -> (p (sine 4.05))
;; -> (p (p (sine 1.35)))
;; -> (p (p (p (sine 0.45))))
;; -> (p (p (p (p (sine 0.15)))))
;; -> (p (p (p (p (p (sine 0.05))))))
;; -> (p (p (p (p (p 0.05)))))
;; p is called at 5-times

;; (sine-wrapper 12.15)
;; (sine-wrapper (* 12.15 10))
;; (sine-wrapper (* 12.15 30))
;; (sine-wrapper (* 12.15 60))
;; (sine-wrapper (* 12.15 90))
;; (sine-wrapper (* 12.15 120))
;; (sine-wrapper (* 12.15 150))
;; (sine-wrapper (* 12.15 180))
;; (sine-wrapper (* 12.15 210))
;; guile> p is called 5-times
;; guile> p is called 7-times
;; guile> p is called 8-times
;; guile> p is called 9-times
;; guile> p is called 9-times
;; guile> p is called 9-times
;; guile> p is called 9-times
;; guile> p is called 10-times
;; guile> p is called 10-times
;; growth in space and number of steps : O(log3 of a)????
