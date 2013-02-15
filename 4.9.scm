
;--------------- eval - apply ------------------

(define true #t)
(define false #f)
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (eval exp env)
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
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
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
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                      (eval (assignment-value exp) env)
                      env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;--------------- grammar ------------------
      
; string, number
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
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


;; ex4.9
; make (while <condition> <body>)

(define (eval exp env)
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
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) 
         (display (let->combination exp))
         (eval (let->combination exp) env))
        ((while? exp)
         (eval-while exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (if (pair? (cadr exp))
      ;; normal let syntax
      (let ((args (cadr exp))
            (body (cddr exp)))
        (let ((vars (map car args))
              (exps (map cadr args)))
          (cons (make-lambda vars body) exps)))
      ;; procedure definition with let
      (let ((var (cadr exp))
            (bindings (caddr exp))
            (body (cadddr exp)))
        (let ((var-args (map car bindings))
              (exps (map cadr bindings)))
          ;; (display var) (display " -- ")
          ;; (display bindings) (display " -- ")
          ;; (display body) (newline)
          (sequence->exp (list (cons (list 'define (cons var var-args) body) exps)
                               (cons var exps)))))))

(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp)
  (cadr exp))
(define (while-body exp)
  (caddr exp))

(define (eval-while exp env)
  (display "condition:") (display (while-predicate exp)) (display " -- ")
  (display "body:") (display (while-body exp)) (newline)
  (if (true? (eval (while-predicate exp) env))
      (begin
        (eval (while-body exp) env)
        (eval exp env))))

(eval-while '(while (= 3 3) (display "OK")) '())


