;----------------------------------------------------
;--------------- my-eval - my-apply------------------

(define true #t)
(define false #f)
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (my-eval exp env)
  (display "my-eval->exp=") (display exp) (newline)
  (display "my-eval->env=") (display env) (newline)
  (cond ((command? exp) (special-command exp env))
        ((self-evaluating? exp) exp)
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
        ((let? exp) 
         (my-eval (let->combination exp) env))
        ((application? exp)
         (my-apply (actual-value (operator exp) env) ;; evaluated-value of operator
                   (operands exp)                    ;; expressions of operands
                   env))                             ;; need to evaluate operands
        (else
         (error "Unknown expression type -- EVAL" exp))))


;; This solution is from
;; http://wqzhang.wordpress.com/2010/04/19/sicp-exercise-4-31/

(define (my-apply procedure arguments env)
  (display "my-apply->procedure=") (display procedure) (newline)
  (display "my-apply->arguments=") (display arguments) (newline)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure                     ; procedure is evaluated at eval
          (list-of-arg-values arguments env))) ; force arguments
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           ;(list-of-delayed-args arguments env)
           (list-of-maybe-delayed-args
            (procedure-parameter-properties procedure)
            arguments env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))


;; normal argument that is without a restrictor has 'normal tag
;; lazy argument has 'simple-thunk tag
;; lazy-memo has 'thunk tag

;; For example, (define (f a (b lazy) c (d lazy-memo)) ...), (f 1 2 3 4) is applied.
;; This solution assumed that the parameter is (('normal 1) ('simple-thunk 2) ('normal 3) ('thunk 4)).
;; When list-of-maybe-delayed-args is called,
;; procedure-parameter-properties returns ('normal 'simple-thunk 'normal 'thunk).
;; Therefore return of list-of-maybe-delayed-args is (1 ('simple-thunk 2) 3 ('thunk 4)).



(define (list-of-maybe-delayed-args props exps env)
  (if (no-operands? exps)
      '()
      (cons (maybe-delay-it (car props) (first-operand exps) env)
            (list-of-maybe-delayed-args (cdr props)
                                        (rest-operands exps)
                                        env))))


(define (maybe-delay-it prop exp env)
  (cond ((eq? prop 'normal)
         (actual-value exp env)) ; find actual value
        ((eq? prop 'lazy)
         (list 'simple-thunk exp env))
        ((eq? prop 'lazy-memo)
         (list 'thunk exp env))
        (else
         (error "Unknown parameter type - MAYBE-DELAY-IT" prop))))

(define (procedure-parameters p)
  (define (extract-real-parameters x)
    (define (extract y)
      (if (pair? y)
          (car y)
          y))
    (if (null? x)
        '()
        (cons (extract (car x))
              (extract-real-parameters (cdr x)))))
  (extract-real-parameters (cadr p)))

(define (procedure-parameter-properties p)
  (define (make-property-list x)
    (define (property y)
      (if (pair? y)
          (cadr y)
          'normal))
    (if (null? x)
        '()
        (cons (property (car x))
              (make-property-list (cdr x)))))
  (make-property-list (cadr p))) ; cadr is to remove 'procedure tag in environment definition of procedure



;--- my adding for debugging ----
; (CMD get-env) : display env
; (CMD get-var <var>); display var

(define (command? exp)
  (tagged-list? exp 'CMD))

(define (special-command exp env)
  (let ((cmd (cadr exp)))
    (cond ((eq? cmd 'get-env)
           (display env))
          ((eq? cmd 'get-var)
           (let ((var (caddr exp)))
             (display (lookup-variable-value var env))))
          (else
           (error "Unknown special command -- CMD" exp))))
  'ok)


(define (actual-value exp env)
  (force-it (my-eval exp env)))


(define (delay-it exp env)
  (list 'thunk exp env))


(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (simple-thunk? obj)
  (tagged-list? obj 'simple-thunk))



;;If it has simple-thunk tag, force-it does not store forced-value.
(define (force-it obj)
  (cond ((simple-thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))
         
                        

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))


(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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


; let
(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((args (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car args))
          (exps (map cadr args)))
      (cons (make-lambda vars body) exps))))
      

; compound procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

;; redefined above
;;(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


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
            ((eq? var (car vars))
             (car vals))
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
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        ;; add here more primitive procedures
        ))

(define (primitive-procedure-names)
  (map car 
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  ;; (display "call primitive proc=>") (display proc) (newline)
  ;; (display "actual values=>") (display args) (newline)
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
    (let ((output (actual-value input the-global-environment)))
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

;---------- end of my-eval and my-apply --------------
(define the-global-environment (setup-environment))
(driver-loop)

;; This solution is from
;; http://wqzhang.wordpress.com/2010/04/19/sicp-exercise-4-31/


(define (f a (b lazy) c (d lazy-memo))
  (+ a b c d))

(f 1 2 3 4)


;; actual result is like following:
;; In the global env, a's value is normal 1,
;; b (simple-thunk 2), c normal 3, d (thunk 4).
;; The eval retuns forced-value of each parameters.

;; my-apply->procedure=(primitive #<primitive-generic +>)
;; my-apply->arguments=(a b c d)
;; my-eval->exp=a
;; my-eval->env=(((a b c d) 1 (simple-thunk 2 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))) 3 (thunk 4 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>))))) ((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))
;; my-eval->exp=b
;; my-eval->env=(((a b c d) 1 (simple-thunk 2 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))) 3 (thunk 4 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>))))) ((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))
;; my-eval->exp=2
;; my-eval->env=(((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))
;; my-eval->exp=c
;; my-eval->env=(((a b c d) 1 (simple-thunk 2 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))) 3 (thunk 4 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>))))) ((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))
;; my-eval->exp=d
;; my-eval->env=(((a b c d) 1 (simple-thunk 2 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))) 3 (thunk 4 (((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>))))) ((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))
;; my-eval->exp=4
;; my-eval->env=(((f false true car cdr cons null? + - * / = eq?) (procedure (a (b lazy) c (d lazy-memo)) ((+ a b c d)) #-6#) #f #t (primitive #<primitive-procedure car>) (primitive #<primitive-procedure cdr>) (primitive #<primitive-procedure cons>) (primitive #<primitive-procedure null?>) (primitive #<primitive-generic +>) (primitive #<primitive-generic ->) (primitive #<primitive-generic *>) (primitive #<primitive-generic />) (primitive #<primitive-generic =>) (primitive #<primitive-procedure eq?>)))

;; ;;; M-Eval value:
;; 10

;; ;;; M-Eval input:


