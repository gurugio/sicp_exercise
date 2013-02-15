
;;a. recursive
(controller
   (assign continue (label expt-done))
   (assign val (const 1))
 expt-recur
   (test (op =) (reg n) (const 0))
   (branch (label expt-ret))
   (save continue)
   (save n)
   (assign continue (label expt-ret))
   (assign n (op -) (reg n) (const 1))
   (goto (label expt-recur))
 expt-ret
   (restore n)
   (restore continue)
   (assign val (op *) (reg val) (reg b))
   (goto (reg continue))
   expt-done)


;; b. iterative

(controller
   (assign product (const 1))
 expt-iter
   (test (op =) (reg counter) (const 0))
   (branch (label expt-ret))
   (assign counter (op -) (reg counter) (const 1))
   (assign product (op *) (reg product) (reg b))
   (goto (label expt-iter))
 expt-done)





