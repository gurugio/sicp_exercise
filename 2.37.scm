(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs))
            )))

(define v1 '((1 2 3 4) (4 5 6 7) (6 7 8 9)))
(define v2 '((10 20 30 40) (40 50 60 70) (60 70 80 90)))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n (lambda (a b) (cons a b)) '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector n mi)) m)))

(dot-product (car v1) (car v2))
(matrix-*-vector v1 (car v1))
(transpose v1)
(transpose v2)
(matrix-*-matrix v1 v2)
