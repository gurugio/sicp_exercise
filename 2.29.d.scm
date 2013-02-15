(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))  ; change

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))  ; change


(define x '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8))))  ; change

(define (terminal-branch? branch)
  (not (pair? (cdr branch)))) ; list?->pair?

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((terminal-branch? mobile) 
         (branch-structure mobile))
        (else
         (+ (total-weight (left-branch mobile))
            (total-weight (right-branch mobile))))))

(define (branch-torque branch)
  (* (branch-length branch) (branch-structure branch)))

(define (mobile-torque mobile)
  (cond ((null? mobile) 0)
        ((terminal-branch? mobile) (branch-torque mobile))
        (else
         (+ (mobile-torque (left-branch mobile))
            (mobile-torque (right-branch mobile))))))

(define (balanced-mobile? mobile)
  (equal? (mobile-torque (left-branch mobile))
          (mobile-torque (right-branch mobile))))



