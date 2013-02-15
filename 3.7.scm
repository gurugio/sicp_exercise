(define (make-account passwd balance)
  (let ((count-pass-error 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (error "FREEZE!!!"))
    (define (dispatch p m)
      (if (eq? passwd p) 
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACOUNT" m)))
          (begin (set! count-pass-error (+ 1 count-pass-error))
                 (if (> count-pass-error 3)
                     (call-the-cops)
                     (error "Incorrect password" count-pass-error)))))
    dispatch))

(define (make-joint acc-name orig-pass new-pass)
  ;; check password with new-pass
  ;; if ok, original account is refered.
  (define (dispatch p m)
    (if (eq? new-pass p)
        (acc-name orig-pass m)
        (error "Incorrect password" p)))
  dispatch)


(define peter-acc (make-account 'open-sesame 100))
((peter-acc 'p 'deposit) 100)
((peter-acc 'open-sesame 'deposit) 100)
((peter-acc 'open-sesame 'withdraw) 40)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'deposit) 100)
((paul-acc 'rosebud 'withdraw) 30)

((paul-acc 'open-sesame 'deposit) 200)
((peter-acc 'open-sesame 'deposit) 10)
