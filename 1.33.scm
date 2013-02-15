
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a b)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))


;;--------------------------------------------------------------------------------------------------------

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

;; changed -> get two arguments but use only one
(define (prime? a b)
  (= a (smallest-divisor a)))


;; 1,2,3,5,7 -> 1+4+9+25+49=88
(filtered-accumulate prime? + 0 square 1 (lambda (x) (+ x 1)) 10)


(define (no-gcd?  a b)
  (= (gcd a b) 1))
(filtered-accumulate no-gcd? ; filter
                     *                            ; combiner
                     1                            ; null-value
                     (lambda (x) x)               ; term
                     1                            ; a
                     (lambda (x) (+ x 1))         ; next
                     10)                          ; b



