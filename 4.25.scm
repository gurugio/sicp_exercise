

(define (unless condition usual-value exception-value)
  (if condition exception-value usual-value))

(define (factorial n)
;;  (display n) (newline)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)

;; guile> ERROR: Stack overflow
;; ABORT: (stack-overflow)
;; guile> 

;; => applicative-order
;; (factorial 5)
;; -> (unless (= 5 1) (* 5 (factorial 4)) 1)
;; -> do (* 5 (factorial 4))
;; -> do (factorial 4)
;; ...
;; (factorial 1) => (unless (= 1 1) (* 1 (factorial 0)) 1)
;; -> try to compute (factorial 0), because argument must be computed first.
;; (factorial 0) => (unless (= 0 1) (* 0 (factorial -1)) 1)
;; -> try to compute (factorial -1)
;; ...
;; try to compute (factorial n) infinitely

;; => normal-order
;; (factorial 5) => (unless (= 5 1) (* 5 (factorial 4)) 1)
;; -> try to call (factorial 4). Inside of factorial procedure is called.
;; -> (unless (= 4 1) (* 4 (factorial 3)) 1)
;; ......
;; -> try to call (factorial 1)
;; -> (unless (= 1 1) ... 1) -> return 1






