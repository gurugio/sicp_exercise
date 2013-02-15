(define x '(((1 2) 3) (4 (5 6))))


;; (define (fringe-body tree result)
;;   (cond ((null? tree) result)
;;         ((list? tree)
;;          (fringe-body (car tree)
;;                       (fringe-body (cdr tree) result)))
;;         (else (cons tree result))))

;; (define (fringe tree)
;;   (fringe-body tree '()))


(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))


(fringe x)


(define (pre-travel tree)
;  (display tree) (newline)
  (cond ((null? tree))
        ((list? tree)
         (pre-travel (car tree))
         (pre-travel (cdr tree)))
        (else (display tree))))


