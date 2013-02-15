(load "eval-apply-ex5.30.scm")


;=========== procedures from other chapter ==============
;; Additional data-structure and syntax procedures for
;; the Scheme evaluator code
;;

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))


;================= machine model =========================

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine))


;; register
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))


;; stack
;; (define (make-stack)
;;   (let ((s '()))
;;     (define (push x)
;;       (set! s (cons x s)))
;;     (define (pop)
;;       (if (null? s)
;;           (error "Empty stack -- POP")
;;           (let ((top (car s)))
;;             (set! s (cdr s))
;;             top)))
;;     (define (initialize)
;;       (set! s '())
;;       'done)
;;     (define (dispatch message)
;;       (cond ((eq? message 'push) push)
;;             ((eq? message 'pop) (pop))
;;             ((eq? message 'initialize) (initialize))
;;             (else (error "Unknown request -- STACK" message))))
;;     dispatch))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))


(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))


;; basic machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:: " name)
            (set! register-table
                  (cons (list name (make-register name)) register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))

      ;------ debugging
      (define (show-registers regs)
        (if (null? regs)
            'done
            (begin
              (let ((reg (car regs)))
                (display (car reg)) (display "=")
                (display (get-contents (cadr reg)))
                (newline))
              (show-registers (cdr regs)))))
      (define (show-instructions insts)
        (if (null? insts)
            'done
            (begin
              (display (car insts)) (newline)
              (show-instructions (cdr insts)))))
      (define (debug-machine cmd)
        (cond ((eq? cmd 'inst) (show-instructions the-instruction-sequence))
              ((eq? cmd 'reg) (show-registers register-table))
              (else (error "Unknown command -- MACHINE-DEBUG" cmd))))
      ;-----------------

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'debug) debug-machine)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;---- debugging ----
(define (debug-machine machine cmd)
  ((machine 'debug) cmd))

;================= assembler =========================

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))


(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))


(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

                        
;=============== execution procedure ====================

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;; assign
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;; test
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; branch
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; goto
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels
                                      (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine
                                    (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; save
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

;; restore
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;; perform
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))


;; subexpression
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknwon operaton -- ASSEMBLE" symbol))))



;;============ explicit-control evaluator ==================

(define-macro (def-op op)
  `(list (quote ,op) ,op))

; ex5.30
(define (unbound-variable? val)
  (eq? val 'unbound))

(define eceval
  (make-machine
   '(exp env val continue proc argl unev)
    (list 
      (def-op self-evaluating?) (def-op variable?) (def-op quoted?)
      (def-op assignment?) (def-op definition?) (def-op if?)
      (def-op lambda?) (def-op begin?) (def-op application?)
      (def-op operator) (def-op operands) (def-op empty-arglist)
      (def-op adjoin-arg) (def-op last-operand?) (def-op no-operands?)
      (def-op first-operand) (def-op rest-operands) (def-op adjoin-arg)
      (def-op primitive-procedure?) (def-op compound-procedure?)
      (def-op apply-primitive-procedure) (def-op procedure-parameters)
      (def-op procedure-environment) (def-op extend-environment)
      (def-op procedure-body) (def-op begin-actions) (def-op first-exp)
      (def-op last-exp?) (def-op rest-exps) (def-op if-predicate)
      (def-op true?) (def-op if-alternative) (def-op if-consequent)
      (def-op assignment-value) (def-op assignment-variable)
      (def-op set-variable-value!) (def-op definition-variable)
      (def-op definition-value) (def-op define-variable!)
      (def-op prompt-for-input) (def-op read) (def-op user-print)
      (def-op get-global-environment) (def-op announce-output)
      (def-op text-of-quotation) (def-op lookup-variable-value)
      (def-op lambda-parameters) (def-op lambda-body)
      (def-op make-procedure) (def-op cond?) (def-op cond->if)
      (def-op cond-actions) (def-op cond-else-clause?) 
      (def-op cond-predicate) (def-op cond-clauses)
      (def-op no-more-exps?)
      (def-op car) (def-op cdr) (def-op null?)
      (def-op unbound-variable?)
      ) ; end of operations
    '(;; controller text
        ;; drive loop

      read-eval-print-loop
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-EVAL input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      print-result
        (perform (op print-stack-statistics)) ;; added for performance checking
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      
        
      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op cond?) (reg exp))
        (branch (label ev-cond-basic))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
        
      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
      ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        ; ex5.30
        (test (op unbound-variable?) (reg val))
        (branch (label unbound-variable))
        (goto (reg continue))
      unbound-variable
        (assign val (const unbound-variable-error))
        (goto (label signal-error))
      ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
      ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (goto (reg continue))
      
      ev-application
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        ;; first we evaluate the operator
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      
      ev-appl-did-operator
        (restore unev)              ; the operands
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val))     ; the operator
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
        
      ev-appl-operand-loop
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      
        ;; When an operand has been evaluated, the value is 
        ;; accumulated into the list held in argl. The operand 
        ;; is then removed from the list of unevaluated operands 
        ;; in unev, and the argument-evaluation continues.
        ;;
      ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
        
        ;; Evaluation of the last argument is handled differently. 
        ;; There is no need to save the environment or the list 
        ;; of unevaluated operands before going to eval-dispatch, 
        ;; since they will not be required after the last operand 
        ;; is evaluated. Thus, we return from the evaluation to a 
        ;; special entry point ev-appl-accum-last-arg, which 
        ;; restores the argument list, accumulates the new 
        ;; argument, restores the saved procedure, and goes off 
        ;; to perform the application.
        ;;
      ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
      ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))
      
        ;; The entry point apply-dispatch corresponds to the 
        ;; apply procedure of the metacircular evaluator. By the 
        ;; time we get to apply-dispatch, the proc register 
        ;; contains the procedure to apply and argl contains the 
        ;; list of evaluated arguments to which it must be 
        ;; applied. The saved value of continue (originally 
        ;; passed to eval-dispatch and saved at ev-application), 
        ;; which tells where to return with the result of the 
        ;; procedure application, is on the stack. When the 
        ;; application is complete, the controller transfers to 
        ;; the entry point specified by the saved continue, with 
        ;; the result of the application in val. As with the 
        ;; metacircular apply, there are two cases to consider. 
        ;; Either the procedure to be applied is a primitive or 
        ;; it is a compound procedure.
        ;;
      apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
        
      primitive-apply
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (restore continue)
        (goto (reg continue))
        
      compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))
        
      ev-begin
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))

        ;; The entries at ev-sequence and ev-sequence-continue 
        ;; form a loop that successively evaluates each 
        ;; expression in a sequence. The list of unevaluated 
        ;; expressions is kept in unev. Before evaluating each 
        ;; expression, we check to see if there are additional 
        ;; expressions to be evaluated in the sequence. If so, 
        ;; we save the rest of the unevaluated expressions 
        ;; (held in unev) and the environment in which these must 
        ;; be evaluated (held in env) and call eval-dispatch to 
        ;; evaluate the expression. The two saved registers 
        ;; are restored upon the return from this evaluation, 
        ;; at ev-sequence-continue.
        ;;
        ;; The final expression in the sequence is handled 
        ;; differently, at the entry point ev-sequence-last-exp. 
        ;; Since there are no more expressions to be evaluated 
        ;; after this one, we need not save unev or env before 
        ;; going to eval-dispatch. The value of the whole 
        ;; sequence is the value of the last expression, so 
        ;; after the evaluation of the last expression there is 
        ;; nothing left to do except continue at the entry point 
        ;; currently held on the stack (which was saved by 
        ;; ev-application or ev-begin.) Rather than setting up 
        ;; continue to arrange for eval-dispatch to return here 
        ;; and then restoring continue from the stack and 
        ;; continuing at that entry point, we restore continue 
        ;; from the stack before going to eval-dispatch, so that 
        ;; eval-dispatch will continue at that entry point after 
        ;; evaluating the expression.
        ;;
      ev-sequence
        ;;ex5.28
        ;; (assign exp (op first-exp) (reg unev))
        ;; (test (op last-exp?) (reg unev))
        ;; (branch (label ev-sequence-last-exp))
        (test (op no-more-exps?) (reg unev))
        (branch (label ev-sequence-end))
        (assign exp (op first-exp) (reg unev))
      
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
      ;; ev-sequence-last-exp
      ;;   (restore continue)
      ;;   (goto (label eval-dispatch))
      ev-sequence-end
        (restore continue)
        (goto (reg continue))

        
        ;; Before evaluating the predicate, we save the if 
        ;; expression itself so that we can later extract the 
        ;; consequent or alternative. We also save the 
        ;; environment, which we will need later in order to 
        ;; evaluate the consequent or the alternative, and we 
        ;; save continue, which we will need later in order 
        ;; to return to the evaluation of the expression that 
        ;; is waiting for the value of the if.
        ;;
      ev-if
        (save exp)                   ; save expression for later
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch)) ; evaluate the predicate
      
        ;; When we return from evaluating the predicate, we 
        ;; test whether it was true or false and, depending on 
        ;; the result, place either the consequent or the 
        ;; alternative in exp before going to eval-dispatch. 
        ;; Notice that restoring env and continue here sets 
        ;; up eval-dispatch to have the correct environment 
        ;; and to continue at the right place to receive the 
        ;; value of the if expression.
        ;;
      ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
      
        ;; Implemented as a derived expression, by converting
        ;; to a nested if
        ;;
      ev-cond
        (assign exp (op cond->if) (reg exp))
        (goto (label ev-if))
      
        ;; Implemented explicitly as a basic special form, 
        ;; without converting to a nested if
        ;;
      ev-cond-basic
        (assign unev (op cond-clauses) (reg exp))
      ev-cond-ev-clause
        (assign exp (op first-exp) (reg unev))
        (test (op cond-else-clause?) (reg exp))
        (branch (label ev-cond-action))
        (save exp)
        (save env)
        (save unev)
        (save continue)
        ;; Setup an evaluation of the clause predicate
        (assign exp (op cond-predicate) (reg exp))
        (assign continue (label ev-cond-clause-decide))
        (goto (label eval-dispatch))
        
      ev-cond-clause-decide
        (restore continue)
        (restore unev)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-cond-action))
      ev-cond-next-clause
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-cond-ev-clause)) ; loop to next clause
        
        ;; We get here when the clause condition was found to
        ;; be true (or it was an 'else' clause), and we want
        ;; the actions to be evaluated. The clause is in exp.
        ;; We setup a call to ev-sequence and jump to it.
        ;;
      ev-cond-action
        (assign unev (op cond-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
      
        ;; Assignments are handled by ev-assignment, which 
        ;; is reached from eval-dispatch with the assignment 
        ;; expression in exp. The code at ev-assignment first 
        ;; evaluates the value part of the expression and 
        ;; then installs the new value in the environment. 
        ;; set-variable-value! is assumed to be available 
        ;; as a machine operation.
        ;;
      ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev)                     ; save variable for later
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch))    ; evaluate the assignment value
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      
        ;; Definitions are handled in a similar way:
        ;;
      ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev)                   ; save variable for later
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch))  ; evaluate the definition value
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
         (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        (restore continue)    ; clean up stack (from apply-dispatch)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
        
      
        
        ))) ;; end of controller text
                 
        

(define the-global-environment (setup-environment))
(define (get-global-environment)
  the-global-environment)


;;;;=========== executing ec-evaluator ============


(start eceval)


(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter producta)
              (+ counter 1))))
  (iter 1 1))

(factorial-iter 2)

;; result: print error message and go back to driver
;; ;;; EC-EVAL input:

;; (total-pushes = 3 maximum-depth = 3)
;; ;;; EC-Eval value:
;; ok

;; ;;; EC-EVAL input:
;; unbound-variable-error

;; ;;; EC-EVAL input:
