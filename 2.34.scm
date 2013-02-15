(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (honer-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms) 
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(honer-eval 2 '(1 3 1 5 1 1))












                      
