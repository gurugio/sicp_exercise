
; this is zero??
(define zero (lambda (f) (lambda (x) x)))
; If (lambda (x) x) == E1, (lambda (f) (E1)) -> E1
; (lambda (x) x) -> x
; Argument of procedure zero is nothing, therefore x -> nothing
; Finally procedure zero is nothing?

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Q1) one = (add-1 zero)
;; (add-1 zero)
;; 1. (n f) = (zero f) = ((lambda (f) (lambda (x) x)) f) = (lambda (x) x)
;; 2. ((n f) x) = ((lambda (x) x) x) = x
;; 3. (f ((n f) x)) = (f x)
;; finally, one = (lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

;; Q2) two = (add-1 one)
;; 1. ((n f) x) = ((one f) x) = (f x)
;; 2. (f (n f) x) = (f (f x))
;; finally, two = (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;;The meaning of Church numerals is the repeat of certain function
((one sqrt) 16) ;; -> (sqrt 16) = 4.0
((two sqrt) 16) ;; -> (sqrt (sqrt 16)) = 2.0


;; I cannot find a direct definition of the +

