

;; (define C (make-connector))
;; (define F (make-connector))
;; (celsius-fahrenheit-converter C F)

;; (define (celsius-fahrenheit-converter c f)
;;   (let ((u (make-connector))
;;         (v (make-connector))
;;         (w (make-connector))
;;         (x (make-connector))
;;         (y (make-connector)))
;;     (multiplier c w u)
;;     (multiplier v x u)
;;     (adder v y f)
;;     (constant 9 w)
;;     (constant 5 x)
;;     (constant 32 y)
;;     'ok))

;; (probe "Celsius temp" C)
;; (probe "Fahrenheit temp" F)

;; (set-value! C 25 'user)
;; (set-value! F 212 'user)
;; (forget-value! C 'user)
;; (set-value! F 212 'user)


(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (averager a b c)
  (let ((sum (make-connector))
        (di (make-connector)))
    (adder a b sum)
    (multiplier sum di c)
    (constant 0.5 di)
    'ok))

(probe "Average" c)
(probe "Num1" a)
(probe "Num2" b)

(averager a b c)

;;test1
(set-value! a 25 'user)
(set-value! b 15 'user)
(get-value c)

;;test2
(set-value! a 15 'user)
(forget-value! a 'user)
(set-value! a 15 'user)

;;test3
(forget-value! a 'user)
(forget-value! c 'user)
(set-value! c 20 'user)

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
           (display "4"))))
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




(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()) (const-names '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
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

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            ((eq? request 'print) (print-const const-names))
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

