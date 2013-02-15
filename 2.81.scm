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


;; ex2.81
(define (exp x y) (apply-generic 'exp x y))


(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

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

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args))) ;; extract tags of each arguments
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args)) ;; extract data
;;           (error
;;             "No method for these types -- APPLY-GENERIC"
;;             (list op type-tags))))))


(define get-coercion get)
(define put-coercion put)



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types (different types)"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


;;---- INSTALL NUMBER_SYSTEM --------------

;; 태그가 2개인 이유 
;; 모든 연산자의 인자는 2개이고 하나의 인자마다 하나의 태그를 가지고 있다.
;; 따라서 (apply-generic) 함수에서 type-tags 변수는 인자들의 태그들의 리스트이므로
;; 2개의 태그 '(scheme-number scheme-number)가 된다.
;; 따라서 테이블에 연산자를 입력할 때 2개의 태그를 타입으로 입력해야 
;; 연산자의 인자들이 연산자에 맞는 인자인지를 알아낼 수 있다.
;; 만약, add 함수에 일반 정수와 유리수를 인자로 넘기면
;; type-tags 변수가 '(scheme-number rational)이 되서 데이터 타입에 오류가 있다는 것을
;; 알아낼 수 있다.


;; normal number
;; eg) (add (make-scheme-number 3) (make-scheme-number 4))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  ;; ex2.81
  (define (scheme-number->complex x)
    (make-complex-from-real-imag (cdr x) 0))

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

  ;; ex2.81
  (put-coercion 'scheme-number 'complex
                scheme-number->complex)

  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

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

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
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

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (square a) (* a a))

;; real + imaginary number
;; 3 + 4i = (rectangular 3 . 4)
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
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
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

  ;; ex2.81
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)


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


  ;; ex2.81
  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)

  'done)

;; complex 숫자들은 2개의 태그를 가진다.
;; make-comple-from-real-imag에서 'complex라는 태그를 추가해주고 
;; make-from-real-imag에서 'rectangular라는 태그를 추가해서
;; (complex rectangular 3 . 4) 와 같은 형태가 된다.
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; MUST INSTALL!!
(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)




;; test
(add (make-scheme-number 2) (make-complex-from-real-imag 2 3))
(exp 2 3)

