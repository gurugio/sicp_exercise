
;; the constructor make-deque, the predicate empty-deque?,
;; selectors front-deque and rear-deque, and
;; mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!.


;; internal procedures

;; pair -> (prev-pair (item next-pair))
(define (make-new-pair item) (cons '() (cons item '())))

(define (set-prev! new-pair prev-pair) (set-car! new-pair prev-pair))
(define (set-next! new-pair next-pair) (set-cdr! (cdr new-pair) next-pair))

(define (get-prev new-pair) (car new-pair))
(define (get-next new-pair) (cddr new-pair))
(define (get-item new-pair) (cadr new-pair))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))


(define aa (make-new-pair 'a))
(define bb (make-new-pair 'b))
(define cc (make-new-pair 'c))


;; aa <-> bb
(set-next! aa bb)
(set-prev! bb aa)
;; bb <-> cc
(set-next! bb cc)
(set-prev! cc bb)
;; result
;; guile> aa
;; (() a #-2# b #-2# c)
;; guile> bb
;; ((() a . #-2#) b #-2# c)
;; guile> cc
;; (((() a . #-2#) b . #-2#) c)
;; guile> (cadr aa)
;; a
;; guile> (cadr bb)
;; b
;; guile> (cadr cc)
;; c
;; guile> (get-item (get-prev bb))
;; a
;; guile> (get-item (get-prev cc))
;; b
;; guile> (get-item (get-next aa))
;; b
;; guile> (get-item (get-next bb))
;; c


;; APIs

(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (cadr (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (cdr (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-new-pair item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-prev! (front-ptr deque) new-pair)
           (set-next! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque))))
        
(define (print-deque deque)
  ;; The first element of deque is a list of elements included in deque
  (let ((temp-front (front-ptr deque)))
    (define (print-deque-body ptr)
      (if (not (null? ptr))
          (begin (display (cadr ptr)) (newline)
                 (print-deque-body (cddr ptr)))))
    (print-deque-body temp-front)))
  

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-new-pair item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-next! (rear-ptr deque) new-pair)
           (set-prev! new-pair (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           deque))))

(define q1 (make-deque))
(front-insert-deque! q1 'c)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'a)

(rear-insert-deque! q1 'c)
(rear-insert-deque! q1 'b)
(rear-insert-deque! q1 'a)

(print-deque q1)

;; result
;; a
;; b
;; c
;; c
;; b
;; a


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (get-next (front-ptr deque)))
         (if (null? (front-ptr deque))
             (set-rear-ptr! deque '()) ;; empty-deque
             (set-prev! (front-ptr deque) '())) ;; unlink old front item
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (get-prev (rear-ptr deque)))
         ;; cannot use empty-deque. rear-ptr is null, but front-ptr is not null yet.
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque '())
             (set-next! (rear-ptr deque) '()))
         deque)))


(define q1 (make-deque))
(front-insert-deque! q1 'c)
(front-delete-deque! q1)
(print-deque q1)

(front-insert-deque! q1 'c)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'a)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1) ;; error

(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'c)
(front-delete-deque! q1)
(front-delete-deque! q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1) ;; error

(define q1 (make-deque))
(front-insert-deque! q1 'c)
(rear-insert-deque! q1 'b)
(front-insert-deque! q1 'a)
(rear-insert-deque! q1 'd) ;; a-c-b-d
(rear-delete-deque! q1) ;; a-c-b
(rear-delete-deque! q1) ;; a-c
(front-delete-deque! q1) ;; c
(front-delete-deque! q1) ;; ()
(print-deque q1)
(empty-deque? q1)




