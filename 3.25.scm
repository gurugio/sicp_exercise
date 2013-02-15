
;; original source
;; (define (make-table)
;;   (let ((local-table (list '*table*)))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (cdr record)
;;                   #f))
;;             #f)))
;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable
;;                             (cons (cons key-2 value)
;;                                   (cdr subtable)))))
;;             (set-cdr! local-table
;;                       (cons (list key-1
;;                                   (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)
;;     (define (print) (display local-table) (newline))
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             ((eq? m 'print-proc) print)
;;             (else (error "Unknown operation -- TABLE" m))))
;;     dispatch))
;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))
;; (define print (operation-table 'print-proc))
;; (put 'math '+ 48)
;; (put 'math '- 45)
;; (put 'math '* 42)
;; (put 'letters 'a 97)
;; (put 'letters 'b 98)
;; (put 'big 'A 67)


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (insert! key-list value)
      (define (insert-body cur-table key-list value)
        (let ((subtable (assoc (car key-list) (cdr cur-table))))
          (if subtable
              (if (null? (cdr key-list)) ;; final key already exists -> key has already data -> change value
                  (set-cdr! subtable value)
                  (insert-body subtable (cdr key-list) value))
              (begin
                (set-cdr! cur-table
                          (cons (list (car key-list))
                                (cdr cur-table)))
                (if (null? (cdr key-list))
                    (set-cdr! (cadr cur-table) value)
                    (insert-body (cadr cur-table) (cdr key-list) value))))))
      (insert-body local-table key-list value))

    (define (lookup key-list)
      (define (lookup-body cur-table key-list)
        (let ((subtable (assoc (car key-list) (cdr cur-table))))
          (if subtable
              (if (null? (cdr key-list))
                  (cdr subtable)
                  (lookup-body subtable (cdr key-list)))
              #f)))
      (lookup-body local-table key-list))

    (define (print) (display local-table) (newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print-proc))

(put '(letters alpha small a) 97)
(get '(letters alpha small a))

(put '(letters alpha small a) 95) ;; change key
(put '(letters alpha small a) 97)
(put '(letters alpha small b) 98)
(put '(letters alpha extra) 99)
(put '(letters alpha big A) 65)
(put '(letters alpha big B) 66)
(put '(letters alpha big C) 67)
(put '(letters num 0) 48)
(put '(letters num 1) 49)
(put '(letters num 2) 50)
(put '(math +) 43)
(put '(math -) 45)
(put '(math *) 42)
;; result
;; (*table* (math (* . 42) (- . 45) (+ . 43)) (letters (num (2 . 50) (1 . 49) (0 . 48)) (alpha (big (C . 67) (B . 66) (A . 65)) (extra . 99) (small (b . 98) (a . 97)))))


(get '(math +))
(get '(letters alpha big B))
(get '(letters alpha small b))
