
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

(flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 10))


(define (check-no-zero num-list)
;  (display num-list) (newline)
  (cond ((null? num-list) #t)
        ((= (car num-list) 0) #f)
        (else  (check-no-zero (cdr num-list)))))
(check-no-zero '(1 2 3))
(check-no-zero '(1 0 2))

(define (prime? n)
  (check-no-zero 
   (map (lambda (i) (remainder n i)) (enumerate-interval 2 (- n 1)))))

(prime? 2)
(prime? 3)
(prime? 4)
(prime? 5)
(prime? 6)
(prime? 7)
(prime? 8)
(prime? 9)
(prime? 10)

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

(prime-sum-pairs 4)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))

(unique-pairs 4)


(define (prime-sum-pairs-ex n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs-ex 4)


(define (generate-triple max)
  (flatmap (lambda (i) ; first number 
             (flatmap (lambda (j) ; second number
                        (map (lambda (k) (list i j k)) ; 3rd number
                             (enumerate-interval 1 i))) ; 3rd number pool
                      (enumerate-interval 1 i))) ; second number pool
           (enumerate-interval 1 max))) ; first number pool

(generate-triple 5)

(define (ex2-41 n s)
  (filter (lambda (t) 
            (if (= (+ (car t) (cadr t) (caddr t)) s) #t
                #f))
          (generate-triple n)))
(ex2-41 5 10)
