#lang scheme
(define parse-table
  (lambda (grammar)
        (print grammar)))

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

