



(rule (can-replace ?p1 ?p2)
      (and (or (and (job ?p1 ?job1)
                    (job ?p2 ?job2)
                    (same ?job1 job2))
               (can-do-job ?job1 ?job2))
           (not (same ?p1 ?p2))))


a.
(can-replace ?person (Cy D. Fect))

b.

(and (can-replace ?person ?high-salary-person)
     (salary ?high-salary-person ?high-salary)
     (salary ?person ?salary)
     (lisp-value > ?high-salary ?salary))
