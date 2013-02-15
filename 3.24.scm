
(define false #f)

(define (my-assoc key records check-equal?)
  (cond ((null? records) false)
        ((check-equal? key (caar records)) (car records)) ;; return pair of data
        (else (my-assoc key (cdr records) check-equal?))))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2 check-key?)
      (let ((subtable (my-assoc key-1 (cdr local-table) check-key?)))
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable) check-key?)))
              (if record
                  (cdr record)  ;; return only data
                  #f))
            #f)))
    (define (insert! key-1 key-2 value check-key?)
      (let ((subtable (my-assoc key-1 (cdr local-table) check-key?)))
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable) check-key?)))
              (if record
                  (set-cdr! record value)  ;; return found data
                  (set-cdr! subtable       ;; add new data into subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table  ;; make new subtable
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

(define (same-key? a b)
  (<= (abs (- a b)) 1))

(put '1 '1 'one (lambda (a b) (= a b)))
(put '1 '4 'two (lambda (a b) (= a b)))
(put '1 '7 'three (lambda (a b) (= a b)))
(put '10 '10 'ten (lambda (a b) (= a b)))
(put '10 '14 'eleven (lambda (a b) (= a b)))
(put '10 '17 'twelve (lambda (a b) (= a b)))


;; #f
(get '1 '5 (lambda (a b) (= a b)))

;; get 'one
(get '1 '1 same-key?)
(get '1 '2 same-key?)
;; get 'two
(get '1 '3 same-key?)
(get '1 '4 same-key?)
(get '1 '5 same-key?)
;; get 'three
(get '1 '6 same-key?)
(get '1 '7 same-key?)
(get '1 '8 same-key?)

