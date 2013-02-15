
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)

1. (try try)의 실행이 멈추는 경우:
 - (try try)의 실행이 멈추기 위해서는 (halts? try try)가 거짓이 되서 'halted가 반환되야 함
 - (halts? try try)가 거짓이 되기 위해서는 halts?가 (try try)의 실행이 멈추지 않는다고 판단해야 함
 - (try try)의 실행이 멈추지만 halts?는 (try try)의 실행이 멈추지 않는다고 판단함
 -> halts? 실패

2. (try try)의 실행이 멈추지 않는 경우:
 - (try try)의 실행이 멈치지 않기 위해서는 (halts? try try)가 참이 되서 (run-forever)가 실행되야 함
 - (halts? try try)가 참이 되기 위해서는 halts?가 (try try)의 실행이 멈춘다고 판단해야 함
 - (try try)의 실행이 멈추지 않지만 halts?는 실행이 멈춘다고 판단함
 -> halts? 실패

result: (try try)의 실행 결과가 어떤 것이든 halts?는 정의와 어긋나게 동작함
