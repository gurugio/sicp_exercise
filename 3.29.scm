

;;a+b = (a'*b')'
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((c1 (make-wire))
          (c2 (make-wire))
          (c3 (make-wire)))
      (inverter a1 c1)
      (inverter a2 c2)
      (and-gate c1 c2 c3)
      (inverter c3 output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
