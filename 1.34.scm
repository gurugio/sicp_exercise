

(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
;; Backtrace:
;; In standard input:
;;  455: 0* [f #<procedure f (g)>]
;;  451: 1  [f 2]
;;  451: 2  [2 2]

;; standard input:451:3: In expression (g 2):
;; standard input:451:3: Wrong type to apply: 2
;; ABORT: (misc-error)
;; guile> 
