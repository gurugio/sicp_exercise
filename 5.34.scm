(load "eval-apply.scm")


(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))


(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))



(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))


(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))


(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))


(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))


(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))


(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))


(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))


(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))


(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))


(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))



(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))


(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define all-regs '(env proc val argl continue))


;---- ex5.34

(define (print-asm result)
  (define (print-lines code)
    (if (not (null? code))
        (begin
          (display (car code))
          (newline)
          (print-lines (cdr code)))))
  (display (list (car result) (cadr result)))
  (newline)
  (print-lines (caddr result)))



(print-asm
 (compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next))

;; ((env) (val))
;; (assign val (op make-compiled-procedure) (label entry1) (reg env))
;; (goto (label after-lambda2))

;; --> entry of factorial procedure
;; entry1
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; (save continue)
;; (save env)

;; --> ending condition
;; (assign proc (op lookup-variable-value) (const =) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch6))
;; compiled-branch7
;; (assign continue (label after-call8))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch6

;; -->test (= n 1)
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call8
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch4))
;; true-branch3
;; (assign val (const 1))
;; (goto (reg continue))
;; false-branch4

;; -->call factorial recursively
;; -->save argument before calling because it is recursive
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (save continue)
;; (save proc)
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op list) (reg val))
;; (save argl)
;; (assign proc (op lookup-variable-value) (const factorial) (reg env))
;; (save proc)
;; (assign proc (op lookup-variable-value) (const -) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch9))
;; compiled-branch10
;; (assign continue (label after-call11))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch9
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call11
;; (assign argl (op list) (reg val))
;; (restore proc)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch12))
;; compiled-branch13
;; (assign continue (label after-call14))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch12
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call14
;; (restore argl)
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch15))
;; compiled-branch16
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch15
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call17
;; after-if5
;; after-lambda2
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))



(print-asm
 (compile
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1))
  'val
  'next))

;; ((env) (val))
;; (assign val (op make-compiled-procedure) (label entry18) (reg env))
;; (goto (label after-lambda19))
;; entry18
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; (assign val (op make-compiled-procedure) (label entry20) (reg env))
;; (goto (label after-lambda21))
;; entry20
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;; (save continue)
;; (save env)
;; (assign proc (op lookup-variable-value) (const >) (reg env))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch25))
;; compiled-branch26
;; (assign continue (label after-call27))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch25
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call27
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch23))
;; true-branch22
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (goto (reg continue))
;; false-branch23

;; --> iter
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; (save continue)
;; (save proc)
;; (save env)

;; --> (+ counter 1)
;; (assign proc (op lookup-variable-value) (const +) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch31))
;; compiled-branch32
;; (assign continue (label after-call33))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch31
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call33
;; (assign argl (op list) (reg val))
;; (restore env)
;; (save argl)

;; --> (* product counter)
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch28))
;; compiled-branch29
;; (assign continue (label after-call30))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch28
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call30
;; (restore argl)
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch34))

;; --> recursively call iter
;; compiled-branch35
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch34
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call36
;; after-if24
;; after-lambda21

;; --> (iter 1 1)
;; (perform (op define-variable!) (const iter) (reg val) (reg env))
;; (assign val (const ok))
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (const 1))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch37))
;; compiled-branch38
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch37
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call39
;; after-lambda19
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))
