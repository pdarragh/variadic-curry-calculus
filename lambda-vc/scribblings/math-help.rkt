#lang racket

(provide (all-defined-out))

(define (separate-text-by-pred text pred)
  (define (group->text-pair group group-matches-pred?)
    (cons group-matches-pred? (list->string (reverse group))))
  (define (separate-chars group group-matches-pred? groups chars)
    (match chars
      [(list)
       (reverse (cons (group->text-pair group group-matches-pred?)
                      groups))]
      [(list c rest-chars ...)
       (if (pred c)
           (if group-matches-pred?
               ;; Add c to existing matching group.
               (separate-chars (cons c group) #t groups rest-chars)
               ;; Start a new matching group.
               (separate-chars (list c)
                               #t
                               (cons (group->text-pair group group-matches-pred?)
                                     groups)
                               rest-chars))
           (if group-matches-pred?
               ;; Start a new non-matching group.
               (separate-chars (list c)
                               #f
                               (cons (group->text-pair group group-matches-pred?)
                                     groups)
                               rest-chars)
               ;; Add c to existing non-matching group.
               (separate-chars (cons c group) #f groups rest-chars)))]))
  (define-values (first-char chars)
    (let ([all-chars (string->list text)])
      (values (first all-chars)
              (rest all-chars))))
  (separate-chars (list first-char) (pred first-char) (list) chars))

(define (textsc text)
  (string-join
   #:before-first "\\mathrm{"
   (map
    (λ (text)
      (string-join
       (map (match-lambda [(cons sc? text)
                           (if sc?
                               (format "\\small{~a}" (string-upcase text))
                               text)])
            (separate-text-by-pred
             text
             char-lower-case?))))
    (string-split text))
   #:after-last "}"))

(define (mk-term term)
  (λ ([sub #f])
    (let ([term-text (format "\\mathtt{\\mathbf{~a}}" term)])
      (if sub
          (format "~a_~a" term-text sub)
          term-text))))

(define t (mk-term "t"))
(define v (mk-term "v"))

(define (mk-rule-name name)
  (string-join
   (map textsc (string-split name "-"))
   (format "\\text{-}")))
