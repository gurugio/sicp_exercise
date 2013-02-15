


(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (branch (label factorial-done))
 (assign t1 (op *) (reg product) (reg counter))
 (assign product (reg t1))
 (assign t2 (op +) (reg counter) (const 1))
 (assign counter (reg t2))
 factorial-done)

