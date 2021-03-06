#lang racket

(provide (all-defined-out))

;; User-defined functions must have formal parameter names that match the
;; regular expression given below. Rendered in English, this correlates to names
;; which begin with an alphabetical character and are then followed by zero or
;; more hyphens or 'word' characters, which include the alphabetical characters,
;; digits, and underscores.
(define (valid-formal? formal)
  (and (symbol? formal)
       (regexp-match #px"^[[:alpha:]](-[[:word:]])*$"
                     (symbol->string formal))))

;; A mix of member and partition. Given a predicate and a list, finds the first
;; element of the list that satisfies the predicate. If found, a triple is
;; returned consisting of the list prior to the element, the element, and the
;; list after the element. If the element is not found, the triple will contain
;; the input list, #f, and an empty list.
(define (member-partition pred lst)
  (define (member-partition processed remaining)
    (match remaining
      [(list)
       (values (reverse processed) #f remaining)]
      [(list item rest-remaining ...)
       (if (pred item)
           (values (reverse processed) item rest-remaining)
           (member-partition (cons item processed) rest-remaining))]))
  (member-partition '() lst))
