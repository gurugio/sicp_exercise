

(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)

(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;; x: a->b-|
;; y: c->d-|
;; z: a->b->c->d-|
(cdr x)  ;; b-|


;; w: a->b->c->d-|
;; x: a->b->c->d-|
;; y: c->d-|
(define w (my-append! x y))
(cdr x)  ;; b->c->d-|
