;========== queue ===================

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define (print-queue queue)
  ;; The first element of queue is a list of elements included in queue
  (let ((temp-front (front-ptr queue)))
    (define (print-queue-body ptr)
      (if (not (null? ptr))
          (begin (display (car ptr)) (newline)
                 (print-queue-body (cdr ptr)))))
    (print-queue-body temp-front)))


;=========== line ===========

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))


;============ gates ===============

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else
         (error "Invalid signal" s1 s2))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else
         (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;============= circuits ===============
      
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


;================== agenda ====================

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))



;============ example =========================

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


;;--------------------- 3.30 ----------------------------------
;; time delay = (length of bit list) * (delay of full-adder)
;;            = 3*or-gate-delay + 4*and-gate-delay + 2*inverter-delay
(define (ripple-carry-adder a-list b-list s-list final-c)
  (let ((c-init (make-wire)))
    (define (ripple-carry-adder-body alist blist slist c-in)
      (let ((c-out (make-wire)))
        (if (not (null? alist))
            (begin
              (full-adder (car alist) (car blist) c-in (car slist) c-out)
              (ripple-carry-adder-body (cdr alist) (cdr blist) (cdr slist) c-out))
            (set-signal! final-c (get-signal c-in)))))
    (ripple-carry-adder-body a-list b-list s-list c-init)))



(define inputa-0 (make-wire))
(define inputa-1 (make-wire))
(define inputa-2 (make-wire))

(define inputb-0 (make-wire))
(define inputb-1 (make-wire))
(define inputb-2 (make-wire))

(define sum0 (make-wire))
(define sum1 (make-wire))
(define sum2 (make-wire))

(define c (make-wire))


(ripple-carry-adder (list inputa-0 inputa-1 inputa-2)
                    (list inputb-0 inputb-1 inputb-2)
                    (list sum0 sum1 sum2)
                    c)

;; (define cout0 (make-wire))
;; (define cin0 (make-wire))
;; (full-adder inputa-0 inputb-0 cin0 sum0 cout0)
;; (define cout1 (make-wire))
;; (full-adder inputa-1 inputb-1 cout0 sum1 cout1)


;; test1: 001+000 = 001 - 0
(get-signal sum0)
(set-signal! inputa-0 1)
(propagate)
(get-signal sum0)
(get-signal sum1)
(get-signal sum2)
(get-signal c)

;; test2: 001+001 = 010 - 0
(set-signal! inputb-0 1)
(propagate)

(get-signal inputa-0)
(get-signal inputa-1)
(get-signal inputb-0)
(get-signal inputb-1)
(get-signal sum0)
(get-signal sum1)
(get-signal sum2)
(get-signal c)

;; test3: 101+001 = 110 - 0
(set-signal! inputa-2 1)
(propagate)
(get-signal sum0)
(get-signal sum1)
(get-signal sum2)
(get-signal c)


;;============== FAILURE : I cannot generate carry! c is now always 0. ================

;; test4: 101+101 = 010 - 1
(set-signal! inputb-2 1)
(propagate)
(get-signal sum0)
(get-signal sum1)
(get-signal sum2)
(get-signal c)

;; test4: 111+101 = 100 - 1
(set-signal! inputa-1 1)
(propagate)
(get-signal sum0)
(get-signal sum1)
(get-signal sum2)
(get-signal c)

