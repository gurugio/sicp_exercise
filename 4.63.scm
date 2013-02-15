

(rule (grand-son ?G ?S)
      (and (son ?F ?S)
           (son ?G ?F)))

(rule (son ?M ?S)
      (and (wife ?M ?W)
           (son ?W ?S)))


;; (son Adam Cain)  
;; (son Cain Enoch)  
;; (son Enoch Irad)  
;; (son Irad Mehujael)  
;; (son Mehujael Methushael)  
;; (son Methushael Lamech)  
;; (wife Lamech Ada)  
;; (son Ada Jabal)  
;; (son Ada Jubal)  
