

(define (cont-frac n d k)
  (define (cont-frac-body count)
    (if (> count k)
        0
        (/ (n count) (+ (d count) (cont-frac-body n d k (+ count 1))))))
  (cont-frac-body 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-iter-body count result)
    (if (= count 0)
        result
        (cont-frac-iter-body (- count 1)
                             (/ (n count)
                                (+ (d count) result)))))
  (cont-frac-iter-body k 0))


(define (get-k k)
  (if (< (abs (- (cont-frac (lambda (i) 1.0)
                            (lambda (i) 1.0)
                            k) target)) 0.00001)
      (begin (display "K is ") (display k) (newline)
             (display target) (display "-") (display (cont-frac (lambda (i) 1.0)
                                                                (lambda (i) 1.0)
                                                                k)) (newline))
      (get-k (+ k 1))))
(define (get-k-iter k)
  (if (< (abs (- (cont-frac (lambda (i) 1.0)
                            (lambda (i) 1.0)
                            k) target)) 0.00001)
      (begin (display "K is ") (display k) (newline)
             (display target) (display "-") (display (cont-frac (lambda (i) 1.0)
                                                                (lambda (i) 1.0)
                                                                k)) (newline))
      (get-k-iter (+ k 1))))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)

(define target (/ 1 (/ (+ 1 (sqrt 5)) 2)))

(get-k 1)
(get-k-iter 1)
