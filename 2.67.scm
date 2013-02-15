
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; example of code-tree
;; ((leaf a 1) (leaf b 2) ab 3)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree)) ; leaf?
      (caddr tree))) ; code-tree?

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree) ; weigh of leaf
      (cadddr tree))) ; weight of code-tree

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)) ; start next decoding
              (decode-1 (cdr bits) next-branch))))) ; next branch
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else
         (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs))) ; make leaf and insert it into the set
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(make-code-tree (make-leaf 'D 1)
                (make-leaf 'C 1))

(make-code-tree (make-leaf 'B 2)
                (make-code-tree (make-leaf 'D 1)
                                (make-leaf 'C 1)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(decode sample-message sample-tree)





















