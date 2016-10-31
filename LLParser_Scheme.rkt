#lang scheme
(define parse-table
  (lambda (grammar)
    (if (= 2 1)
        1
        ((display grammar) ))))


;used to test how a funtion with lambda is supposed to behave
(define reverse-subtract
  (lambda (x y) (- y x)))

(define calc-gram
'(("P" ("SL" "$$"))
  ("SL" ("S" "SL") ())
  ("S" ("id" ":=" "E") ("read" "id") ("write" "E"))
  ("E" ("T" "TT"))
  ("T" ("F" "FT"))
  ("TT" ("ao" "T" "TT") ())
  ("ao" ("+") ("-"))
  ("mo" ("*") ("/"))
  ("F" ("id") ("num") ("(" "E" ")"))
))
