


;; 난수 수열을 만들기가 어려우므로
;; 1 2 4 8 16... 수열을 난수로 가정하고 처리한다.

(define random-init 1)

(define (rand-update seed)
  (* seed 2))
  
(define rand  ;; 함수가 아니라 객체다! 따라서 x값은 계속 유지된다.
  (let ((x random-init))
    (define (dispatch cmd)
      (cond ((eq? cmd 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? cmd 'reset)
             ;; 변수를 추가로 요청해서 새로 값을 시작함
             (lambda (new-val) (set! x new-val)))
            (else
             (error "Unknown command"))))
    dispatch))
            
(rand 'generate)
((rand 'reset) 3)
