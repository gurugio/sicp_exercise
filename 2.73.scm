

;;----------------------------------
;; ch 3.3.3
;;----------------------------------
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;;-------------------------------------------------------------

;;---------------------------------------------------------
;; ch 2.3.2
;;---------------------------------------------------------
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (addend s) (car s))
(define (augend s) (cadr s))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base s) (car s))
(define (exponent s) (cadr s))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))
;;---------------------------------------------------------

;; ex2.73 (b)
(define (deriv-add exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))
(define (deriv-mul exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(put 'deriv '+ deriv-add)
(put 'deriv '* deriv-mul)


(deriv '(+ x y) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x) ;(+ (* x y) (* y (+ x 3)))

;; ex2.73 (c)
(define (deriv-expo exp var)
  (make-product
   (make-product (exponent exp)
                 (make-exponentiation (base exp)
                                      (- (exponent exp) 1)))
   (deriv (base exp) var)))

(put 'deriv '** deriv-expo)

(deriv '(** x 3) 'x)
(deriv '(** y 3) 'x)
(deriv '(** 3 3) 'x)
(deriv '(** x 0) 'x)
(deriv '(** x 1) 'x)


;; ex2.73 (d)
(define (deriv-reverse exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         ((get (operator exp) 'deriv) (operands exp) var))))

;; If put has arguments of type and operator, not operator and type,
;; it does not effect other codes.
(put '+ 'deriv deriv-add)
(deriv-reverse '(+ x y) 'x)
