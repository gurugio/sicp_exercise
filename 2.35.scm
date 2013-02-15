(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves '((((1 2) (3 4))) ((5 6))))

;; copy from solution of scheme-wiki
;; I don't understand this
(define (count-leaves-recursive t) 
  (accumulate + 0 (map (lambda (node) 
                         (if (pair? node) 
                             (count-leaves-recursive node) 
                             1)) 
                       t))) 

(count-leaves-recursive '((((1 2) (3 4))) ((5 6))))
