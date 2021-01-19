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

;; Like findf, but instead of only returning the found item (or #f) this version
;; returns the item and the index at which it was found (or #f if not found).
(define (findf+index proc lst)
  (define (findf+index lst index)
    (match lst
      [(list)
       #f]
      [(list item rest-lst ...)
       (if (proc item)
           (cons item index)
           (findf+index rest-lst (add1 index)))]))
  (findf+index lst 0))
