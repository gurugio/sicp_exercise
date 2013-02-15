(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-ex p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (square x)
  (* x x))
(define (append-ex seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-ex sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
