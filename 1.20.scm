

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; R = remainder

;; normal-order evaluation
;; (gcd 206 40)
;; (gcd 40 (R 206 40))
;; (gcd (R 206 40) (R 40 (R 206 40))
;; (gcd (R 40 (R 206 40)) (R (R 206 40) (R 40 (R 206 40))))
;; (gcd (R (R 206 40) (R 40 (R 206 40)))
;;      (R (R 40 (R 206 40)) (R (R 206 40) (R 40 (R 206 40)))))
;; R -> 21-times

;; applicative-order evaluation
;; (gcd 206 40)
;; (gcd 40 (R 206 40)) = (gcd 40 6)
;; (gcd 6 (R 40 6)) = (gcd 6 4)
;; (gcd 4 (R 6 4)) = (gcd 4 2)
;; (gcd 2 (R 4 2)) = (gcd 2 0) = 2
;; R -> 4-times
