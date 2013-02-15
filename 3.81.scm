;; for Guile implementation
(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


;; user native force of Guile
;; (define (force delayed-object)
;;   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-map proc . argstreams)
  (if (stream-null? (stream-car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))


(define (display-stream strm count)
  (define (body strm n)
    (if (< n 0) 'done
        (begin (display (stream-car strm)) (newline)
               (body (stream-cdr strm) (- n 1)))))
  (body strm count))



(define (stream-rand cmd numbers)
  (cond ((eq? cmd 'generate)
         
        ((eq? cmd 'reset)
         (stream-rand (cons-stream random-init (stream-map rand-update numbers))))
        (else
         (error "Unknown command"))))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

; example of rand-update
(define random-init-ex 1)
(define (rand-update-ex seed)
  (+ seed 3))


;; the first practice

;; (define (stream-rand rand-update last-num)
;;   (define (dispatch cmd)
;;     (cond ((eq? cmd 'generate)
;;            (cons-stream last-num
;;                         ((stream-rand rand-update (rand-update last-num)) 'generate)))
;;           ((eq? cmd 'reset)
;;            (lambda (new-number)
;;              (cons-stream new-number
;;                           ((stream-rand rand-update new-number) 'generate))))))
;;   dispatch)

;; (define my-rand
;;   (stream-rand rand-update-ex random-init-ex))
                                    
;; (display-stream (my-rand 'generate) 5)
;; (display-stream ((my-rand 'reset) 2) 5)
;; (display-stream ((my-rand 'reset) 3) 5)
;; guile> 2
;; 2
;; 5
;; 8
;; 11
;; 14
;; done
;; guile> 3
;; 3
;; 6
;; 9
;; 12
;; 15
;; done



;;--------------------------------------------------------------------
;; advance solution (other's solution on Net)

(define (stream-rand numbers)
  (define (dispatch cmd num)
    (cond ((eq? cmd 'g)
           (rand-update-ex num))
          (else
           cmd))) ; cmd is new initial number
  (cons-stream random-init-ex
               (stream-map dispatch numbers (stream-rand numbers)))) ; dispatch takes s0 and generated numbers


; Command generating random number is 'g
; normal number indicates new initial number
(define s-generate (cons-stream 'g s-generate))
(define s0 (cons-stream 'g
                        (cons-stream 'g
                                     (cons-stream 3
                                                  (cons-stream 'g
                                                               s-generate)))))

(display-stream (stream-rand s-generate) 10)
(display-stream (stream-rand s0) 10)
;; guile> 1
;; 4
;; 7
;; 10
;; 13
;; 16
;; 19
;; 22
;; 25
;; 28
;; 31
;; done
;; guile> 1
;; 4
;; 7
;; 3
;; 6
;; 9
;; 12
;; 15
;; 18
;; 21
;; 24
;; done
;; guile> 
;;--------------------------------------------------------------------
