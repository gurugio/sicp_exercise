
;;------------------------------------------------
;;
;; I failed to solve 2.32!
;; This code is not working.
;;
;;------------------------------------------------


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
                    
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (check-no-zero num-list)
  (cond ((null? num-list) #t)
        ((= (car num-list) 0) #f)
        (else  (check-no-zero (cdr num-list)))))

(define (prime? n)
  (check-no-zero 
   (map (lambda (i) (remainder n i)) (enumerate-interval 2 (- n 1)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))


(define (prime-sum-pairs-ex n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;================  ex2.42 ===================

(define success-case '((0 5) (1 2) (2 0) (3 6) (4 4) (5 7) (6 1) (7 3)))
(define fail-case1 '((0 5) (1 2) (1 0) (3 6) (4 4) (5 7) (6 1) (7 3)))
(define fail-case2 '((0 5) (1 2) (2 0) (2 6) (4 4) (5 7) (6 1) (7 3)))

 
(define (adjoin-position new-row new-col other-queens)
  (cons (list new-row new-col) other-queens))

(adjoin-position 3 4 '((1 2) (2 2) (3 3)))

(define empty-board '())

(define (equal-value? value other-values)
  (cond ((null? other-values) #f)
        ((= value (car other-values)) #t)
        (else (equal-value? value (cdr other-values)))))
(equal-value? 3 '(1 2 4 5 6))
(equal-value? 3 '(1 2 3 4 5))

(define (all-different-value? values)
  (cond ((null? values) #t)
        ((equal-value? (car values) (cdr values)) #f)
        (else (all-different-value? (cdr values)))))
(all-different-value? '(1 2 3 4 5))
(all-different-value? '(1 2 3 3 5))

(define (safe-row? board-size queens)
  (let ((row-list (map (lambda (pair) (car pair)) queens)))
    (all-different-value? row-list)))
(safe-row? 8 success-case)

(define (same-diagonal? queen1 queen2)
  (= (abs (- (car queen1) (car queen2)))
          (abs (- (cadr queen1) (cadr queen2)))))
(same-diagonal? '(1 2) '(4 5))
(same-diagonal? '(4 5) '(1 2))
(same-diagonal? '(1 2) '(4 3))

(define (safe-diagonal? board-size queens)
  (cond ((null? queens) #t)
        ((same-diagonal? (car queens) (cadr queens)) #f)
        (else (safe-diagonal? board-size queens))))
(safe-diagonal? 8 success-case)

(define (safe? board-size queens)
  (and (safe-row? board-size queens)
       (safe-diagonal? board-size queens)))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
