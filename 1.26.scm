
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; (expmod base (/ exp 2) m) is called two times.
;; Therefore the number of process is not decreased.
;; Finally the number of process is O(n).
