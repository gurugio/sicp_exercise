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
;; (put <op> <type> <item>)
;; (get <op> <type>
;;------------------------------------------------


;;----- GENERIC PROCEDURES -----------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (ex-equ? x y) (apply-generic 'ex-equ? x y))
(define (ex-zero? x) (apply-generic 'ex-zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
(define (add-3 a b c) (apply-generic 'add-3 a b c))

;; ex2.88
(define (negation x) (apply-generic 'negation x))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (raise x) (apply-generic 'raise x))

(define (project z)
  (apply-generic 'project z))

(define (install-get-higher-system)
  (put 'scheme-number 'rational 'rational)
  (put 'scheme-number 'complex 'complex)
  (put 'rational 'scheme-number 'rational)
  (put 'rational 'complex 'complex)
  (put 'complex 'scheme-number 'complex)
  (put 'complex 'rational 'complex))
(install-get-higher-system)       

(define (former-is-higher-system? type1 type2)
  (cond ((eq? type1 type2) #f)
        ((and (eq? type1 'rational) (eq? type2 'scheme-number)) #t)
        ((eq? type1 'complex) #t)
        (else #f)))

(define (drop x)
  (cond ((eq? (type-tag x) 'scheme-number) x)
        ((ex-equ? (raise (project x)) x) (drop (project x)))
        (else x)))


;; fail to make new apply-generic which is applied with drop procedure
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;; extract tags of each arguments
    (let ((proc (get op type-tags)))
      (if proc
          ;; I tried (drop (apply proc (map contents args))),
          ;; but it generated Stack-Overflow.
          ;; I failed to make new apply-generic procedure.
          (apply proc (map contents args)) ;; extract data
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (former-is-higher-system? type1 type2)
                    (apply-generic op (car args) (raise (cadr args)))
                    (apply-generic op (raise (car args)) (cadr args))))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))))


;;------ Number Systems ----------------------

;; normal number
;; eg) (add (make-scheme-number 3) (make-scheme-number 4))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define (scheme-number->complex x)
    (make-complex-from-real-imag (cdr x) 0))
  (define (scheme-number->rational x)
    (make-rational x 1))


  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'ex-equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'ex-zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'add-3 '(scheme-number scheme-number scheme-number)
       (lambda (a b c) (tag (+ a b c))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

  (put 'raise '(scheme-number) scheme-number->rational)


  ;; ex2.88
  (put 'negation '(scheme-number)
       (lambda (x) (tag (- x))))

  'done)


(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rational number
;; eg) (add (make-rational 3 4) (make-rational 3 4))
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (ex-equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (ex-zero? x)
    (= (numer x) 0))

  (define (rational->complex x)
    (make-complex-from-real-imag (round (/ (numer x) (denom x))) 0))

  (define (tag x) (attach-tag 'rational x))

  (define (project-rational x) ; rational -> scheme-number
    (make-scheme-number (round (/ (numer x) (denom x)))))


  ;; interface to rest of the system
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'ex-equ? '(rational rational) ex-equ?)
  (put 'ex-zero? '(rational) ex-zero?)
  (put 'raise '(rational) rational->complex)
  (put 'project '(rational) project-rational)

  (put 'negation '(rational)
       (lambda (x) (tag (make-rat ( - (numer x))
                                  (denom x)))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))



;; real + imaginary number
;; 3 + 4i = (rectangular 3 . 4)
(define (square a) (* a a))
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; mag + angle number
;; 3 + 45' = (polar 3 . 45)
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; complex number
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (define (add-complex-3 z1 z2 z3)
    (make-from-real-imag (+ (real-part z1) (real-part z2) (real-part z3))
                         (+ (imag-part z1) (imag-part z2) (imag-part z3))))

  (define (project-complex z)
    (make-rational (round (real-part z)) 1))

  (put 'add-3 '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex-3 z1 z2 z3))))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'ex-equ? '(complex complex)
       (lambda (z1 z2) (and (equal? (real-part z1) (real-part z2))
                            (equal? (imag-part z1) (imag-part z2)))))
  (put 'ex-zero? '(complex)
       (lambda (z1) (and (equal? (real-part z1) 0)
                         (equal? (imag-part z1) 0))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'project '(complex) project-complex)

  (put 'negation '(complex)
       (lambda (z) (display "귀찮아서 안만듬")))

  'done)

;; complex 숫자들은 2개의 태그를 가진다.
;; make-comple-from-real-imag에서 'complex라는 태그를 추가해주고 
;; make-from-real-imag에서 'rectangular라는 태그를 추가해서
;; (complex rectangular 3 . 4) 와 같은 형태가 된다.
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)



;;---------- ch2.5.3 -------------------------------

(define (install-polynomial-package)
  ;; from ch2.3.2
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (zero-termlist? terms)
    (cond ((empty-termlist? terms)
           #t)
          ((zero? (coeff (first-term terms)))
           (zero-termlist? (rest-terms terms)))
          (else
           #f)))


  (define (adjoin-term term term-list)
    (if (ex-zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))


  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
                     
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
             
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) ; order of term
                      (mul (coeff t1) (coeff t2))) ; coeff of term
           (mul-term-by-all-terms t1 (rest-terms L))))))



  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                             (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  
  ;; ex2.88
  (define (negation-termlist termlist)
    (if (empty-termlist? termlist) '()
        (adjoin-term 
         (make-term (order (first-term termlist))
                    ; use negation again that can handle any type of coeff
                    (negation (coeff (first-term termlist))))
         (negation-termlist (rest-terms termlist)))))


  (define (tag p) (attach-tag 'polynomial p))



  ;; ex2.89
  (define (make-dense-from-sparse sparse-termlist)
    (if (empty-termlist? sparse-termlist) '()
        (make-dense-from-sparse-body (order (first-term sparse-termlist))
                                     sparse-termlist
                                     '())))
  (define (make-dense-from-sparse-body current-order termlist newlist)
    (cond ((< current-order 0) newlist)
          ((empty-termlist? termlist)
           (make-dense-from-sparse-body
            (- current-order 1)
            termlist
            (append newlist (list 0))))
          ((= current-order (order (first-term termlist)))
           (make-dense-from-sparse-body
            (- current-order 1)
            (rest-terms termlist)
            (append newlist (list (coeff (first-term termlist))))))
          (else
           (make-dense-from-sparse-body
            (- current-order 1)
            termlist
            (append newlist (list 0))))))


  ;; install operations
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p2 p1))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ;; term-list를 반드시 호출해야
  ;; 변수가 없어지고 각 term을 곧바로 처리할 수 있게 된다.
  (put 'ex-zero? '(polynomial) 
       (lambda (terms) (zero-termlist? (term-list terms))))

  ;; ex2.88
  ;; 
  (put 'negation '(polynomial)
       (lambda (termlist)
         (tag (make-poly (variable termlist)
                         (negation-termlist (term-list termlist))))))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (add (tag p1)
                            (negation (tag p2)))))
  ; negation은 generic이므로 호출을 위해서는 태그가 필요하다.
  ; 하지만 sub가 호출되면서 태그가 벗겨지므로 다시 태그를 붙여준다.
  ; 그리고 p1도 태그를 다시 붙여줘야 generic한 add를 호출할 수 있다.
  ; p1에 태그를 안붙이고 negation 결과에 태그를 붙여서
  ; add-poly를 호출해도 된다.


  (put 'make-dense-from-sparse '(polynomial) 
       (lambda (x) (tag (make-poly (variable x)
                                   (make-dense-from-sparse (term-list x))))))
       

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))




                 
;;--------- test ----------------

;; MUST INSTALL!!
(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-polynomial-package)


(define test-termlist (make-polynomial 'x '((100 1) (2 2) (0 1))))
(add test-termlist test-termlist)
(mul test-termlist test-termlist)

(define (make-dense-from-sparse x)
  (apply-generic 'make-dense-from-sparse x))
                                      
(make-dense-from-sparse test-termlist)
(make-dense-from-sparse (make-polynomial 'x '((100 1))))

;; both of followings should be the same
(make-dense-from-sparse (make-polynomial 'x '((100 0))))
(make-dense-from-sparse (make-polynomial 'x '()))
