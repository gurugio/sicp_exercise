;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) 
;;               (cons (square (car things)) -> new item is located first
;;                     answer))))
;;   (iter items nil))

;; (define (square-list items)
;;   (define (square x) (* x x))
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer -> (cons list atom) is not plain list
;;                     (square (car things))))))
;;   (iter items nil))

(define (square-list items)
  (define (square x) (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items nil))
