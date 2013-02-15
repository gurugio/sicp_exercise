


(rule (big-shot ?person)
      (or (and (supervisor ?person ?boss)
               (lisp-value null? ?boss))
          (and (supervisor ?person ?boss)
               (job ?person ?my-job)
               (job ?boss ?boss-job)
               (not (same ?boss-job ?my-job)))))



