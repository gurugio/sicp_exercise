

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))

(set-to-wow! z1)
(set-to-wow! z2)
;; guile> ((wow b) wow b)
;; guile> ((wow b) a b)



    ;; z1.........
    ;;         . .
    ;;         . .
    ;;         . .
    ;;         . .
    ;;  x.................
    ;;         .         .
    ;;         .         .
    ;;         .         .
    ;;         wow       b



    ;;  z2......................
    ;;      .          .       .
    ;;      .          .       .
    ;;      .          .       .
    ;;      .          a       b
    ;;      .                  .
    ;;      .                  .
    ;;      .                  .
    ;;      ....................
    ;;                 .
    ;;                 .
    ;;                 .
    ;;                wow


