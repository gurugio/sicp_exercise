

(define (make-monitored proc)
  (let ((called-count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) called-count)
            ((eq? m 'reset-count) (set! called-count 0))
            (else 
             (begin
               (set! called-count (+ 1 called-count))
               (proc m)))))
    mf))

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(s 4)
(s 16)
(s 'reset-count)


;; let으로 만든 지역변수의 값이 계속 남아있는 이유는
;; (define ss ...)으로 다시한번 ss라는 객체로 정의되어서이다.
;; ss가 존재하는한 let으로 만든 지역변수도 계속 존재하게 된다.
;; 만약 객체로 정의하지 않고 한번만 실행되는 함수안에 있는 지역변수라면
;; 실행될때마다 변수가 생성되므로 계속 선언된 값만 가지게 된다.
(define (addnum proc)
  (let ((result 0))
    (define (mf n)
      (set! result (proc result n))
      result)
    mf))

(define (addnum2 proc)
  (let ((result 0))
    (define (mf n)
      (set! result (proc result n))
      (display "RESULT=") (display result) (newline))
    mf))

(define ss (addnum +))

(ss 1) ; ss객체는 계속 유지되므로 result 변수는 값이 바뀜?
(ss 2)
((addnum2 +) 1) ;; 실행될때마다 result변수가 생성되므로 계속 1이 됨
