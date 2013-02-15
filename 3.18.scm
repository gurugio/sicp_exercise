

(define (my-last-pair x)
  (if (null? (cdr x))
      x
      (my-last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (my-last-pair x) x)
  x)

(define z (make-cycle '(a b c d e f g)))


(define (cycle? pair-list)
  (define (go-fast lst)
    (cdr (cdr lst)))
  (define (go-slow lst)
    (cdr lst))
  (define (travel-list fast slow)
;    (display fast) (display "--") (display slow) (newline)
    (cond ((equal? (car fast) (car slow))
           #t)
          ((or (null? (go-slow fast)) (null? (go-fast fast)))
           #f)
          (else
           (travel-list (go-fast fast) (go-slow slow)))))
  (travel-list (go-fast pair-list) (go-slow pair-list)))


