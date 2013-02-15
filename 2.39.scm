(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define s '(1 2 3 4 5))

(define (reverse-ex sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(reverse-ex s)

(define (reverse-ex2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(reverse-ex2 s)
