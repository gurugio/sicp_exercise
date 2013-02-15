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

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;; extract tags of each arguments
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args)) ;; extract data
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


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
  'done)


(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;; MUST INSTALL!!
(install-scheme-number-package)


;;-------- ex 2.78 ---------------

;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; (define (contents datum)
;;   (if (pair? datum) (cdr datum)
;;       (error "Bad tagged datum -- CONTENTS" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

; test
(if (and (eq? (cdr (add 1 2)) 3) (eq? (car (add 1 2)) 'scheme-number)) '#t
    (display "FAIL TO SOLVE EX2.78"))

(add 3 (add (make-scheme-number 3) (make-scheme-number 4)))
