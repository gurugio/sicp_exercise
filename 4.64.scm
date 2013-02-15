
(outranked-by Ben ?boss) ==>

(or (supervisor Ben Oliver) -> find Oliver
    (and (outranked-by ?middle-manager Oliver) -> outranked-by is called AGAIN
         (...)))

(outranked-by ?staff-person Oliver) ==>

(or (supervisor ?staff-person Oliver)   -> ?staff-person can have many choices
    (and (outranked-by ?middle-manager Oliver) -> outranked-by is called AGAIN
         (...)))

(outranked-by ?staff-person Oliver) ==>

(or (supervisor ?staff-person Oliver)
    (and (outranked-by ?middle-manager Oliver) -> outranked-by is called AGAIN
         (...)))

RESULT> (outranked-by ?staff-person Oliver) is repeated.
