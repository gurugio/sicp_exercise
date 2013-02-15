
;; (define (last-pair lst)
;;   (if (null? (cdr lst))
;;       lst  --------------------> (1)
;;       (last-pair (cdr lst)))) -> (2)

;; (1)
(rule (last-pair (?z) (?z)))

;; (2)
(rule (last-pair (?y . ?x) ?z)
      (last-pair (?x) ?z))


;;http://d.hatena.ne.jp/rsakamot/20090707/1246973376
(assert! (rule (last-pair (?x . ()) (?x))))  ;rule1
(assert! (rule (last-pair (?x . ?y) ?z)      ;rule2
               (last-pair ?y ?z)))

;;내가 한거랑 일본사람이한거랑 어느게 맞나?
;;아래는 일본사람 테스트 결과
;; ;;; Query input:
;; (last-pair (3) ?x)
;; ;;; Query results:
;; (last-pair (3) (3))

;; ;;; Query input:
;; (last-pair (1 2 3) ?x)
;; ;;; Query results:
;; (last-pair (1 2 3) (3))

;; ;;; Query input:
;; (last-pair (2 ?x) (3))
;; ;;; Query results:
;; (last-pair (2 3) (3))

;; ;;; Query input:
;; (last-pair ?x (3))
