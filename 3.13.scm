

(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (my-last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))


;; calling built-in last-pair results error
;; standard input:123:1: In procedure last-pair in expression (last-pair z):
;; standard input:123:1: Circular structure in position 1: (a b c . #-2#)
;; ABORT: (misc-error)
(last-pair z)

;; custom last-pair do infinite-loop
(my-last-pair z)
