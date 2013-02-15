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


;;------------------------------------------------
;; data files
;; Each file has a division name at the first item,
;; and can have its own data format.
;;------------------------------------------------

;; file of division A
(define file-of-division-a
  '(division-a ; division name
  ;;;name, salary, phone
    (name-a-1 111 1111)
    (name-a-2 222 2222)
    (name-a-3 333 3333)))

;; file of division B
(define file-of-division-b
  '(division-b
  ;; phone, name, addr, salary
    (3333 name-b-1 aaaa 333)
    (4444 name-b-2 bbbb 444)
    (5555 name-b-3 cccc 555)))

;; file of division C
(define file-of-division-c
  '(division-c
  ;; name, salary
    (name-c-1 666)
    (name-c-2 777)
    (name-c-3 888)
    (name-c-4 999)))


;;------------------------------------------------
;; Methods for each division
;; Employee name must be unique so that it can be used as key.
;; Every method must have one argument that employee name
;; and return result value at success case,
;; otherwise false.
;;------------------------------------------------


(define (install-division-a)
  ;;; methods

  ;; get record of name or return false
  (define (get-record name)
    (define (get-name-from-record record)
      (car record))
    (define (find-record-by-name records name)
      (cond ((null? records) #f)
            ((eq? name (get-name-from-record (car records)))
             (car records))
            (else
             (find-record-by-name (cdr records) name))))
    (find-record-by-name (cdr file-of-division-a) name))

  ;; get salary of name or return false
  (define (get-salary name)
    (if (get-record name) (cadr (get-record name))
        #f))

  ;;; install methods
  (put 'get-record 'division-a get-record)
  (put 'get-salary 'division-a get-salary)

  'done)

(install-division-a)

(define (install-division-b)
  ;;; methods
  ;; get record of name or return false
  (define (get-record name)
    (define (get-name-from-record record)
      (cadr record))
    (define (find-record-by-name records name)
      (cond ((null? records) #f)
            ((eq? name (get-name-from-record (car records)))
             (car records))
            (else
             (find-record-by-name (cdr records) name))))
    (find-record-by-name (cdr file-of-division-b) name))

  ;; get salary of name or return false
  (define (get-salary name)
    (if (get-record name) (cadddr (get-record name))
        #f))

  ;;; install methods
  (put 'get-record 'division-b get-record)
  (put 'get-salary 'division-b get-salary)
  'done)

(define (install-division-c)
  ;;; methods
  ;; get record of name or return false
  (define (get-record name)
    (define (get-name-from-record record)
      (car record))
    (define (find-record-by-name records name)
      (cond ((null? records) #f)
            ((eq? name (get-name-from-record (car records)))
             (car records))
            (else
             (find-record-by-name (cdr records) name))))
    (find-record-by-name (cdr file-of-division-c) name))

  ;; get salary of name or return false
  (define (get-salary name)
    (if (get-record name) (cadr (get-record name))
        #f))

  ;;; install methods
  (put 'get-record 'division-c get-record)
  (put 'get-salary 'division-c get-salary)

  'done)

(install-division-b)
(install-division-c)

;; simple version of apply-generic
;; (define (apply-generic op name)
;;   (cond (((get op 'division-a) name) ((get op 'division-a) name));try file-of-division-a
;;         (((get op 'division-b) name) ((get op 'division-b) name))
;;         (((get op 'division-c) name) ((get op 'division-c) name))
;;         (else #f)))

(define (apply-generic op name)
  (let ((result-division-a ((get op 'division-a) name)))
    (if result-division-a result-division-a
        (let ((result-division-b ((get op 'division-b) name)))
          (if result-division-b result-division-b
              (let ((result-division-c ((get op 'division-c) name)))
                (if result-division-c result-division-c
                    (display "fail"))))))))
  
;;-------------------------
;; generic functions
;;-------------------------
(define (get-record name) (apply-generic 'get-record name))
(define (get-salary name) (apply-generic 'get-salary name))

;; test
(get-record 'name-a-1)
(get-record 'name-a-2)
(get-record 'name-b-2)
(get-record 'name-c-3)

(get-record 'a)

(get-salary 'name-a-1)
(get-salary 'name-a-2)
(get-salary 'name-b-2)
(get-salary 'name-c-3)

(get-salary 'a)
