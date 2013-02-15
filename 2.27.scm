
(define (reverse lst)
  (if (and (list? lst) (not (null? lst)))
      (append (reverse (cdr lst)) (list (car lst)))
      lst))

(define (deep-reverse lst)
  (display lst) (newline)
  (if (and (list? lst) (not (null? lst)))
      (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst))))
      lst))
