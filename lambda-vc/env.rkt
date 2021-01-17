#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Environments
;;
;; Environments in lambda-vc are just lists of pairs:
;;   (list ... (cons variable-name variable-value) ...)
;; To extend an environment requires only to cons a new pair onto the front of
;; the list. This allows us to avoid many non-pathological cases of hygiene.

;; The empty environment is just an empty list.
(define mt-env empty)
(define env-empty? empty?)
;; Extending an environment requires consing a new pair on the head of the
;; current environment.
(define (extend-env name value env)
  (cons (cons name value) env))
;; We look up variables' values in the environment by performing an in-order
;; search for the variable name. By doing it this way, inner variables that are
;; identical to outer variables simply shadow, avoiding difficulties introduced
;; by other lookup schemes.
(define (env-lookup env name)
  (match env
    [(list) #f]
    [(cons (cons l-name l-value) rest-env)
     (if (equal? name l-name)
         l-value
         (env-lookup rest-env name))]))
