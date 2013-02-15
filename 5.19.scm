
;=========== procedures from other chapter ==============
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))



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
  (let ((contents '*unassigned*)
        ;;ex5.18
        (trace-status '#f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ;;ex5.18
            ((eq? message 'trace-on)
             (set! trace-status '#t))
            ((eq? message 'trace-off)
             (set! trace-status '#f))
            ((eq? message 'set)
             (lambda (value)
               (if (eq? trace-status '#t)
                   (begin
                     (display (list "NAME:" name "VAL:" contents "->" value)) (newline)))
               (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;; test ex5.18
;; (define ddd (make-register 'ddd))
;; (ddd 'trace-on)
;; (set-contents! ddd 1)
;; (get-contents ddd)
;; (ddd 'trace-off)


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
        ;; ex5.15
        (count-exe-inst 0)
        ;; ex5.16
        (trace-status '#f)
        ;; ex5.17
        (cur-label '*unassigned*)
        ;; ex5.19
        (cur-label-offset 0)
        ;; list of break points like ((fact-loop 3) (after-fact 2))
        (break-points '())
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

      ;;ex5.15
      (define (init-count-exe-inst)
        (set! count-exe-inst 0))
      (define (inc-count-exe-inst)
        (set! count-exe-inst (+ count-exe-inst 1)))

      ;;ex5.16
      (define (start-trace)
        (set! trace-status '#t))
      (define (stop-trace)
        (set! trace-status '#f))

      ;; ex5.17
      (define (is-label? inst)
        (eq? (caar inst) 'label))

      ;; ex5.18
      (define (start-reg-trace reg-name)
        (let ((reg (cadr (assoc reg-name register-table))))
          (reg 'trace-on)))
      (define (stop-reg-trace reg-name)
        (let ((reg (cadr (assoc reg-name register-table))))
          (reg 'trace-off)))

      ;; ex5.19
      (define (set-break-point label num)
        (set! break-points (cons (list label num) break-points)))
      (define (del-break-point label num)
        (define (substract-break-point bp-lst scan-lst)
          (if (null? bp-lst)
              scan-lst
              (if (and (eq? label (car (car bp-lst)))
                       (eq? num (cadr (car bp-lst))))
                  (substract-break-point (cdr bp-lst) scan-lst)
                  (substract-break-point (cdr bp-lst) (cons (car bp-lst) scan-lst)))))
        (set! break-points (substract-break-point break-points '())))
      (define (scan-break-point bp-lst scan-bp)
        (if (null? bp-lst)
            '#f
            (if (and (eq? (car scan-bp) (car (car bp-lst)))
                     (eq? (cadr scan-bp) (cadr (car bp-lst))))
                '#t
                (scan-break-point (cdr bp-lst) scan-bp))))
      (define (cancel-break-point)
        (set! break-points '()))
      (define (get-break-point)
        break-points)
      
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ;; ex5.17,ex5.19
                (if (is-label? (car insts))
                    (begin
                      (set! cur-label (cadaar insts)) ; label is not executing-instruction
                      (set! cur-label-offset 0))
                    (begin
                      (inc-count-exe-inst)
                      (set! cur-label-offset (+ cur-label-offset 1))))
                (if (eq? trace-status '#t)
                    (begin
                      (display (list "LABEL:" cur-label " INST:" (caar insts))) (newline)))

                ;; ex5.19
                (if (scan-break-point break-points (list cur-label cur-label-offset))
                    (begin
                      (display (list "BREAK!:" (caar insts))) (newline))
                    (begin
                      (set! cur-label-offset (+ cur-label-offset))
                      ((instruction-execution-proc (car insts)))
                      (execute)))))))

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
              ;;ex5.15
              ((eq? message 'count-exe-inst) count-exe-inst)
              ((eq? message 'init-count-exe-inst) (init-count-exe-inst))
              ;;ex5.16
              ((eq? message 'trace-on) (start-trace))
              ((eq? message 'trace-off) (stop-trace))
              ;;ex5.18
              ((eq? message 'reg-trace-on) start-reg-trace)
              ((eq? message 'reg-trace-off) stop-reg-trace)
              ;;ex5.19
              ((eq? message 'set-bp) set-break-point)
              ((eq? message 'del-bp) del-break-point)
              ((eq? message 'get-bp) (get-break-point))
              ((eq? message 'cancel-bp) (cancel-break-point))
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
               (let ((insts
                      (cons (list (list 'label next-inst)) insts))) ;; ex5.17
                 (receive insts
                     (cons (make-label-entry next-inst
                                             insts)
                           labels)))
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
        ((eq? (car inst) 'label)
         (lambda () (advance-pc pc)))
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




;;===================== ex5.18 =========================
(define facto-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1! = 1
   (goto (reg continue))                   ; return to caller
 fact-done)))


(set-register-contents! facto-machine 'n 5)
;; (facto-machine 'get-bp)
((facto-machine 'set-bp) 'fact-loop 3)
((facto-machine 'set-bp) 'fact-loop 5)
((facto-machine 'set-bp) 'after-fact 2)

;; pass del test
;; ((facto-machine 'del-bp) 'fact-loop 5)
;; (facto-machine 'get-bp) ;ok
;; ((facto-machine 'del-bp) 'fact-loop 3)
;;(facto-machine 'cancel-bp)

(facto-machine 'trace-on)
(start facto-machine)

;; result: breaking executing
;; (LABEL: *unassigned*  INST: (assign continue (label fact-done)))
;; (LABEL: fact-loop  INST: (label fact-loop))
;; (LABEL: fact-loop  INST: (test (op =) (reg n) (const 1)))
;; (LABEL: fact-loop  INST: (branch (label base-case)))
;; (LABEL: fact-loop  INST: (save continue))
;; (BREAK!: (save continue))


;; I skipped proceed-machine becauseof lack of time.

