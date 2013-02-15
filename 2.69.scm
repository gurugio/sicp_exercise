

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

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

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs))) ; make leaf and insert it into the set
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
;;  (display message) (newline)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol data tree)
  (define (is-tree? sub-tree)
    (pair? sub-tree))
  (define (encode-symbol-body data sub-tree bits)
;;    (display sub-tree) (newline)
    (cond ((not (is-tree? sub-tree)) (error "Unknown data"))
          ((and (leaf? sub-tree) (equal? (list data) (symbols sub-tree)))
           bits)
          ((and (not (leaf? sub-tree)) (equal? (symbols (left-branch sub-tree)) (list data)))
           (append bits '(0)))
          (else
           (encode-symbol-body data
                               (right-branch sub-tree)
                               (append bits '(1))))))
  (encode-symbol-body data tree '()))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; In result tree, most high and left branch must be E
(define (successive-merge leaves)
  (define (successive-merge-body right-branch remain-leaves)
;;    (display remain-leaves) (display "-") (display right-branch) (newline)
    (if (null? remain-leaves) right-branch
        (successive-merge-body (make-code-tree (car remain-leaves) right-branch)
                               (cdr remain-leaves))))
  (successive-merge-body (car leaves) (cdr leaves)))

(define sample-pairs 
  '((C 3) (A 5) (B 4) (D 1)))
(define sample-tree (generate-huffman-tree sample-pairs))

;; test successive-merge with encode
(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)

(encode '(A B C D) sample-tree)
(encode '(A C A B B D A) sample-tree)
(encode '(A B C D A E D B) (successive-merge sample-pairs))
