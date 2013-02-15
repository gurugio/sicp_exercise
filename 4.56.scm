

a. (and (supervisor ?name (Bitdiddle Ben))
        (address ?name ?where))

b. (and (salary (Bitdiddle Ben) ?Ben-amount)
        (salary ?name ?amount)
        (lisp-value > ?amount ?Ben-amount))

c. (and (supervisor ?target-name ?super-name)
        (not (job ?super-name (computer . ?type)))
        (job ?super-name ?do))


