

(define var -1)

(define (f num)
    (if (eq? var -1)
        (begin (set! var num)
               var)
        0))

(+ (f 0) (f 1)) ;; 0

;; right term first is the same with (+ (f 1) (f 0))
(+ (f 1) (f 0)) ;; 1
    


        
