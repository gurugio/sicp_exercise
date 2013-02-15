

(define a (make-connector "a"))
(define b (make-connector "b"))


(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a) ;; b has no value, but a
            (set-value! b
                        (* (get-value a) (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          ((eq? request 'Who-are-you)
           (display "I-am-squarer") (newline))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a me 'squarer)
  (connect b me 'sqaurer)
  me)
            

(squarer a b)

(set-value! a 10 'user)
;; (a 'get-informant)
;; user
;; guile> (b 'get-informant)
;; #<procedure me (request)>
;; guile> ((b 'get-informant) 'Who-are-you)
;; I-am-squarer

(get-value a)
(get-value b)

(forget-value! a 'user)
(get-value a)
(get-value b)
(a 'print)
(b 'print)

(set-value! b 4 'user)
(get-value a)
(get-value b)
(a 'print)
(b 'print)

(set-value! b 100 'user) ;; constradiction
(forget-value! b 'user)
(set-value! b 100 'user)
(get-value a)
(get-value b)
(a 'print)
(b 'print)



;===== system ======

;; (has-value? <connector>)
;; tells whether the connector has a value.
;; (get-value <connector>)
;; returns the connector's current value.
;; (set-value! <connector> <new-value> <informant>)
;; indicates that the informant is requesting the connector to set its value to the new value.
;; (forget-value! <connector> <retractor>)
;; tells the connector that the retractor is requesting it to forget its value.
;; (connect <connector> <new-constraint>)
;; tells the connector to participate in the new constraint.


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (display "1")
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (display "2")
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (display "3")
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))
          (else
           (display "cannot set new value"))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          ((eq? request 'Who-are-you)
           (display "I-am-adder") (newline))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me 'adder)
  (connect a2 me 'adder)
  (connect sum me 'adder)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          ((eq? request 'Who-are-you)
           (display "I-am-multiplier") (newline))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me 'multi)
  (connect m2 me 'multi)
  (connect product me 'multi)
  me)


(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me 'constant)
  (set-value! connector value me)
  me)


(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me 'probe)
  me)




(define (make-connector name)
  (let ((value #f) (informant #f) (constraints '()) (const-names '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval informant)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (display name) (display " lost value") (newline)
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))
    (define (connect new-constraint new-name)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))

      (if (not (memq new-name const-names))
          (set! const-names
                (cons new-name const-names)))
      'done)

    (define (print-const names)
      (if (null? names) 'done
          (begin
            (display "CONSTRAINTS:")
            (display (car names))
            (newline)
            (print-const (cdr names)))))
    (define (print-informant)
      (display "INFORMANT:") (display informant) (newline))

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            ((eq? request 'print)
             (print-const const-names)
             (print-informant))
            ((eq? request 'get-informant)
             informant)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))



(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))



(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint new-name)
  ((connector 'connect) new-constraint new-name))

