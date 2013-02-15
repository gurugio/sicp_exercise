;----------------------------------------------------
;--------------- my-eval - my-apply------------------

(define true #t)
(define false #f)
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (my-eval exp env)
  (display "my-eval:") (display exp) (newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))

        ;; add by ex4.6
        ((let? exp) 
         (display (let->combination exp))
         (my-eval (let->combination exp) env))

        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure arguments))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)


;--------------- grammar ------------------
      
; string, number
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? exp '#t) true)
        ((eq? exp '#f) true)
        (else false)))

; variable
(define (variable? exp) (symbol? exp))

; quoted expression
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;; Every expression is passed as quoted form like '(*).
;; Therefore quoted expression is interpreted like followings.
;; '('a)
;; ((quote a))
;; (car '('a))
;; (quote a)

; assignment: (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


; definition
; 1. (define <var> <value>)
; 2. (define (<var> <parameter1> ...) <body>)
; 2'. (define <var>
;      (lambda <parameter1> ...)
;       <body>))
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)  ; parameter
                   (cddr exp)))) ; body


; lambda expression
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; combine sequential expression into one begin expression
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq)) ; single expression
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; procedure
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        ; else로 시작하는지만 확인해서 아니면 if를 만든다.
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error ("ELSE cluase isn't last -- COND->IF" clauses)))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; compound procedure
(define (make-procedure parameters body env)
;  (list 'procedure parameters body env))
  (list 'procedure parameters (scan-out-defines body) env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) 
  (caddr p))
(define (procedure-environment p) (cadddr p))

; let
(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((args (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car args))
          (exps (map cadr args)))
      (cons (make-lambda vars body) exps))))
      

; environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; variables
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))

            ;; ex4.16.a
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unbound variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
    

; setup running environment

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


; setup primitive procedure

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; add here more primitive procedures
        (list '+ +)
        (list '* *)
        (list '- -)
        ))

(define (primitive-procedure-names)
  (map car 
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;----------------------------------------
;; Must call apply of native scheme implementation
;; I am using 'guile' and its apply is 'apply'
;; I changed produre names eval and apply
;; in this file into my-eval, my-apply,
;; so that they are not confused with native apply and eval.
(define apply-in-underlying-scheme apply)
;----------------------------------------


; read-eval-print loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))



;; ex4.16.b
(define (scan-out-defines exps)
  (display "scan-out-defines gets:") (display exps) (newline)
  (let ((vars '())
        (vals '()))
    (define (extract-defines exps)
      (if (not (null? exps))
          (if (tagged-list? (car exps) 'define)
              (begin
                (set! vars (append vars (list (cadr (car exps)))))
                (set! vals (append vals (list (caddr (car exps)))))
                (extract-defines (cdr exps)))
              (extract-defines (cdr exps)))
          '()))
    (define (make-args vars)
      (if (not (null? vars))
          (cons (list (car vars) (quote '*unassigned*))
                (make-args (cdr vars)))
          '()))
    (define (make-sets vars vals)
      (if (not (null? vars))
          (cons (list 'set! (car vars) (car vals))
                (make-sets (cdr vars) (cdr vals)))
          '()))
    (define (extract-no-definition-body exps)
      (if (not (null? exps))
          (if (tagged-list? (car exps) 'define)
              (extract-no-definition-body (cdr exps))
              (cons (car exps)
                    (extract-no-definition-body (cdr exps))))
          '()))

    (extract-defines exps)
    (display "vars:") (display vars) (newline)
    (if (null? vars)
        exps
        (list (append (list 'let (make-args vars))
                      (make-sets vars vals)
                      (extract-no-definition-body exps))))))
    

;; test scan-out-defines
(scan-out-defines '((define u 11) (define v 22) (+ u v) (* v u)))
(scan-out-defines '(+ u v))



(define the-global-environment (setup-environment))
(driver-loop)

(define u 1)
(define v 3)
(define (ddd) (+ u v))
(ddd)

(let ((a 3)
      (b 4))
  (+ a b))

(define (ddd) (define u 11) (define v 22) (+ u v) (* v u))
(ddd)


;; test result

;; guile> scan-out-defines gets:((define u 11) (define v 22) (+ u v) (* v u))
;; vars:(u v)
;; ((let ((u (quote *unassigned*)) (v (quote *unassigned*))) (set! u 11) (set! v 22) (+ u v) (* v u)))
;; guile> scan-out-defines gets:(+ u v)
;; vars:()
;; (+ u v)
;; guile> guile> 

;; ;;; M-Eval input:
;; my-eval:(define u 1)
;; my-eval:1

;; ;;; M-Eval value:
;; ok

;; ;;; M-Eval input:
;; my-eval:(define v 3)
;; my-eval:3

;; ;;; M-Eval value:
;; ok

;; ;;; M-Eval input:
;; my-eval:(define (ddd) (+ u v))
;; my-eval:(lambda () (+ u v))
;; scan-out-defines gets:((+ u v))
;; vars:()

;; ;;; M-Eval value:
;; ok

;; ;;; M-Eval input:
;; my-eval:(ddd)
;; my-eval:ddd
;; my-eval:(+ u v)
;; my-eval:+
;; my-eval:u
;; my-eval:v

;; ;;; M-Eval value:
;; 4

;; ;;; M-Eval input:
;; my-eval:(let ((a 3) (b 4)) (+ a b))
;; ((lambda (a b) (+ a b)) 3 4)my-eval:((lambda (a b) (+ a b)) 3 4)
;; my-eval:(lambda (a b) (+ a b))
;; scan-out-defines gets:((+ a b))
;; vars:()
;; my-eval:3
;; my-eval:4
;; my-eval:(+ a b)
;; my-eval:+
;; my-eval:a
;; my-eval:b

;; ;;; M-Eval value:
;; 7

;; ;;; M-Eval input:
;; my-eval:(define (ddd) (define u 11) (define v 22) (+ u v) (* v u))
;; my-eval:(lambda () (define u 11) (define v 22) (+ u v) (* v u))
;; scan-out-defines gets:((define u 11) (define v 22) (+ u v) (* v u))
;; vars:(u v)

;; ;;; M-Eval value:
;; ok

;; ;;; M-Eval input:
;; my-eval:(ddd)
;; my-eval:ddd
;; my-eval:(let ((u (quote *unassigned*)) (v (quote *unassigned*))) (set! u 11) (set! v 22) (+ u v) (* v u))
;; ((lambda (u v) (set! u 11) (set! v 22) (+ u v) (* v u)) (quote *unassigned*) (quote *unassigned*))my-eval:((lambda (u v) (set! u 11) (set! v 22) (+ u v) (* v u)) (quote *unassigned*) (quote *unassigned*))
;; my-eval:(lambda (u v) (set! u 11) (set! v 22) (+ u v) (* v u))
;; scan-out-defines gets:((set! u 11) (set! v 22) (+ u v) (* v u))
;; vars:()
;; my-eval:(quote *unassigned*)
;; my-eval:(quote *unassigned*)
;; my-eval:(set! u 11)
;; my-eval:11
;; my-eval:(set! v 22)
;; my-eval:22
;; my-eval:(+ u v)
;; my-eval:+
;; my-eval:u
;; my-eval:v
;; my-eval:(* v u)
;; my-eval:*
;; my-eval:v
;; my-eval:u

;; ;;; M-Eval value:
;; 242

;; ;;; M-Eval input:

