

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))

(define w (mystery v))

;; result
;; guie> w
;; (d c b a)
;; guile> v
;; (a)


;; v: a->b->c->d
;; (loop (a b c d) ()) -> temp=(b c d), x=(a)---> this x is v!
;; (loop (b c d) (a)) -> temp=(c d), x=(b a)
;; (loop (c d) (b a)) -> temp=(d), x=(c b a)
;; (loop (d) (c b a)) -> temp=(), x=(d c b a)
;; (loop () (c d b a)) -> returns (c d b a)
