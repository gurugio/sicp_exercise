;; (define (front-ptr queue) (car queue))
;; (define (rear-ptr queue) (cdr queue))
;; (define (set-front-ptr! queue item) (set-car! queue item))
;; (define (set-rear-ptr! queue item) (set-cdr! queue item))
;; (define (empty-queue? queue) (null? (front-ptr queue)))
;; (define (front-queue queue)
;;   (if (empty-queue? queue)
;;       (error "FRONT called with an empty queue" queue)
;;       (car (front-ptr queue))))
;; (define (insert-queue! queue item)
;;   (let ((new-pair (cons item '())))
;;     (cond ((empty-queue? queue)
;;            (set-front-ptr! queue new-pair)
;;            (set-rear-ptr! queue new-pair)
;;            queue)
;;           (else
;;            (set-cdr! (rear-ptr queue) new-pair)
;;            (set-rear-ptr! queue new-pair)
;;            queue))))
;; (define (delete-queue! queue)
;;   (cond ((empty-queue? queue)
;;          (error "DELETE! called with an empty queue" queue))
;;         (else
;;          (set-front-ptr! queue (cdr (front-ptr queue)))
;;          queue)))
;; (define (print-queue queue)
;;   ;; The first element of queue is a list of elements included in queue
;;   (let ((temp-front (front-ptr queue)))
;;     (define (print-queue-body ptr)
;;       (if (not (null? ptr))
;;           (begin (display (car ptr)) (newline)
;;                  (print-queue-body (cdr ptr)))))
;;     (print-queue-body temp-front)))


;; (define (make-queue) (cons '() '()))
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ;; How to porting old funtions
    ;; - delete argument queue
    ;; - change front-ptr,rear-ptr functions into local variables

    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (print-queue)
      ;; The first element of queue is a list of elements included in queue
      (let ((temp-front front-ptr))
        (define (print-queue-body ptr)
          (if (not (null? ptr))
              (begin (display (car ptr)) (newline)
                     (print-queue-body (cdr ptr)))))
        (print-queue-body temp-front)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
      
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr)))))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?)
             (empty-queue?))
            ((eq? m 'front-queue)
             (front-queue))
            ((eq? m 'insert)
             insert-queue!)
            ((eq? m 'print)
             (print-queue))
            ((eq? m 'delete)
             (delete-queue!))
            (else
             (error "Unidentified command" m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))
(define (insert-queue! queue item)
  ((queue 'insert) item))
(define (delete-queue! queue)
  (queue 'delete))
(define (print-queue queue)
  (queue 'print))

(define q2 (make-queue))

(empty-queue? q2)
(insert-queue q2 'a)
(insert-queue q2 'b)
(insert-queue q2 'c)
(insert-queue q2 'd)

(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(empty-queue? q2)


