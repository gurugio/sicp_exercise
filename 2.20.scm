
(define (same-parity a . b)
  (if (even? a) (even-numbers (cons a b))
      (odd-numbers (cons a b))))

(define (even-numbers numbers)
  (cond ((null? numbers) '())
        ((even? (car numbers))
         (append (list (car numbers)) (even-numbers (cdr numbers))))
        (else
         (append (even-numbers (cdr numbers))))))

(define (odd-numbers numbers)
  (cond ((null? numbers) '())
        ((odd? (car numbers))
         (append (list (car numbers)) (odd-numbers (cdr numbers))))
        (else
         (append (odd-numbers (cdr numbers))))))

