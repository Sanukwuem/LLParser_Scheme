#lang scheme

#|
;Sean Anukwuem
;Evan Melquist
:Project 2 - part 2
|#

(define sort
  (lambda (L)
    ; Use string comparison to quicksort list.
    (letrec ((partition
              (lambda (e L A B)
                (if (null? L) (cons A B)
                    (let ((c (car L)))
                      (if (string<? c e)
                          (partition e (cdr L) (cons c A) B)
                          (partition e (cdr L) A (cons c B))))))))
      (cond
        ((null? L) L)
        ((null? (cdr L)) L)
        (else (let* ((AB (partition (car L) (cdr L) '() '()))
                     (A (car AB))
                     (B (cdr AB)))
                (append (sort A)
                        (list (car L))
                        (sort B))))))))


(define unique
  (lambda (L)
    ; Return list in which adjacent equal elements have been combined.
    (cond
      ((null? L) L)
      ((null? (cdr L)) L)
      ((equal? (car L) (cadr L)) (unique (cdr L)))
      (else (cons (car L) (unique (cdr L)))))))


(define unique-sort
  (lambda (L)
    ; Sort (using string-ified elements) and remove duplicates.
    (unique (sort L))))


(define flatten
  (lambda (L)
    ; Return left-to-right fringe of tree as list.
    (cond
      ((null? L) L)
      ((list? (car L)) (append (flatten (car L)) (flatten (cdr L))))
      (else (cons (car L) (flatten (cdr L)))))))


(define start-symbol
  (lambda (grammar)
    (caar grammar)))

(define last
  (lambda (L)
    ; Return last element of list.
    (cond
      ((null? L) '())
      ((null? (cdr L)) (car L))
      (else (last (cdr L))))))

(define end-marker
  (lambda (grammar)
    (last (cadar grammar))))

(define non-terminals
  (lambda (grammar)
    ; Return list of all non-terminals in grammar.
    (map car grammar)))

(define gsymbols
  (lambda (grammar)
    ; Return list of all symbols in grammar (no duplicates).
    (unique-sort (flatten grammar))))

(define non-terminal?
  (lambda (x grammar)
    ; Is x a non-terminal?
    (not (not (member x (non-terminals grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define terminals
  (lambda (grammar)
    ; Return list of all terminals in grammar.
    (apply append
           (map (lambda (x) (if (non-terminal? x grammar) '() (list x)))
                (gsymbols grammar)))))

(define productions
  (lambda (grammar)
    ; Return list of all productions in grammar.
    ; Each is represented as a (lhs rhs) pair, where rhs is a list.
    (apply append
           (map (lambda (prods)
                  (map (lambda (rhs)
                         (list (car prods) rhs))
                       (cdr prods)))
                grammar))))


(define gsymbol?
  (lambda (x grammar)
    ; is x a symbol in grammar?
    ; (note that name symbol? is taken by Scheme)
    (not (not (member x (gsymbols grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define terminal?
  (lambda (x grammar)
    ; Is x a terminal in grammar?
    (and (member x (gsymbols grammar))
         (not (member x (non-terminals grammar))))))

(define union
  (lambda sets
    (unique-sort (apply append sets))))

(define initial-knowledge
  (lambda (grammar)
    ; Return knowledge structure with empty FIRST and FOLLOW sets
    ; and false gen-epsilon estimate for all symbols.
    (map (lambda (A) (list A #f '() '()))
         (non-terminals grammar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Custom Functions Start Here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Test Grammar
(define calc-gram 
'(("P" ("SL" "$$"))
  ("SL" ("S" "SL") (" "))
  ("S" ("id" ":=" "E") ("read" "id") ("write" "E"))
  ("E" ("T" "TT"))
  ("T" ("F" "FT"))
  ("TT" ("ao" "T" "TT") (" "))
  ("FT" ("mo" "F" "FT") (" "))
  ("ao" ("+") ("-"))
  ("mo" ("*") ("/"))
  ("F" ("id") ("num") ("(" "E" ")"))
))

;If the input is equal to the non-terminal specified then get that specific line
(define getProduction
  (lambda (w grammar)
    (if (eqv? (car (car grammar))w)
    (car grammar)
    (if (eqv? (car (car (cdr grammar)))w)
        (car (cdr grammar))
        (if (eqv? (car (car (cdr (cdr grammar))))w)
            (car (cdr (cdr grammar)))
            (if (eqv? (car (car (cdr (cdr (cdr grammar)))))w)
                (car (cdr (cdr (cdr grammar))))
                (if (eqv? (car (car (cdr (cdr (cdr (cdr grammar))))))w)
                    (car (cdr (cdr (cdr (cdr grammar)))))
                    (if (eqv? (car (car (cdr (cdr (cdr (cdr (cdr grammar)))))))w)
                        (car (cdr (cdr (cdr (cdr (cdr grammar))))))
                        (if (eqv? (car (car (cdr (cdr (cdr (cdr (cdr (cdr grammar))))))))w)
                            (car (cdr (cdr (cdr (cdr (cdr (cdr grammar)))))))
                            (if (eqv? (car (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar)))))))))w)
                                (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar))))))))
                                (if (eqv? (car (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar))))))))))w)
                                    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar)))))))))
                                    (if (eqv? (car (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar)))))))))))w)
                                        (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr grammar))))))))))
                                        (print "no match")))))))))))))
#|
    ; if w is a terminal, return (w)
    ; if w is a non-terminal, look it up and then call first on the first element of the list
    ; if w is an empty list, return ()
    ; if w is a non-empty list, "iterate" over elements
    ; if no match exists then the function prints no match
|#
(define first
  (lambda (w knowledge grammar)
    (if (eqv? w " ")
    "()"
    (if (terminal? w grammar)
        w
        (if (non-terminal? w grammar)
            (for-each (lambda (x)  (print (first x knowledge grammar ))(display " ") ) (map car (cdr (getProduction w grammar))))
                (print "no match"))))))

(define parse-table
  (lambda (knowledge grammar)
    (for-each (lambda (x) (print x)  (display ": ") (print (first x knowledge grammar)) (newline)) (map car grammar))
    ))

;;;;;;;;;;;;;;;;;;;; Function Calls ;;;;;;;;;;;;;;;;;
;(getProduction "T" calc-gram)
;(first "SL" (initial-knowledge calc-gram) calc-gram)
;(parse-table (initial-knowledge calc-gram) calc-gram)