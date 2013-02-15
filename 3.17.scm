



;; 한번 읽은 쌍의 포인터를  리스트로 모아서
;; 중복된 포인터를 제거하고 나머지의 갯수를 센다.

(define pairs-list '())

(define (insert-pair target-list pair)
  (cond ((null? target-list) (set! pairs-list (cons pair pairs-list)))
        ((equal? (car target-list) pair) 
         #f)
        (else
         (insert-pair (cdr target-list) pair))))


(define (count-pairs x)
  (if (not (pair? x))
      0
      (begin (insert-pair pairs-list (car x)) ;; Which is correct, (car x) or x?
             (count-pairs (car x))
             (count-pairs (cdr x))
             (length pairs-list))))
      


;;; test shared data
(define z '(a))

;; pairs-list=(a (a)) , result=2
(define pairs-list '())
(count-pairs (cons z (cons z '())))

;; pairs-list=(a (a) ((a) a)), result=3
(define pairs-list '())
(count-pairs (cons (cons z z) (cons z z)))

;; pairs-list=(c b a), result=3
(define pairs-list '())
(count-pairs '(a b c))

;; all followings are 3
(count-pairs '((a) b))
(count-pairs '(a b c))
(count-pairs '(((a))))
(count-pairs '((a b)))
(count-pairs '(a (b)))
(count-pairs '(() (a)))
