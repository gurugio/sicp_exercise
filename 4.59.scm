


a.
(meeting ?division (Friday ?time))


b.
(rule (meeting-time ?person ?day-and-time)
      (or (and (job ?person (?division .?))
               (meeting ?division ?day-and-time))
          (meeting whole-company ?day-and-time)))



                    
c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
