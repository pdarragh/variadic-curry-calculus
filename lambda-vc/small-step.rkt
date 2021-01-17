#lang racket

(provide (all-defined-out))

(require "env.rkt")

;; All "values" are sub-types of the value struct. This way, we can use the
;; predicate `value?` to determine all values.
(struct value ())

;; Error messages are wrapped in structs so superposition collapsing can avoid
;; installing error handlers.
(struct err-value value (message) #:transparent)

;; TODO
(struct superposition-value value (lhs rhs))

;; Superpositions are just two grouped inner expressions.
(struct superposition (lhs rhs)
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (match s
       [(superposition lv rv)
        (write-string (format "(σ ~a ~a)" lv rv)
                      port)]))])

;; Attempts to perform a single-step reduction of `exp`. Returns a pair:
;;
;; 1. The returned expression.
;; 2. `#f` if the returned expression is identical to the input expression
;;    (i.e., if no reduction occurred), or else a symbol naming the rule
;;    responsible for the reduction.
;;
;; NOTE: If multiple rules are applied in one step, the rules are returned as a
;;       list with the inner-most rules to the end. (This is still a "small
;;       step" since only a single reduction is performed.)
(define (step exp env)
  (define (return-change new-exp rule)
    (values new-exp rule))
  (define (return-no-change)
    (values exp #f))
  (define (recur-step sub-exp change-func outer-rule)
    ;; This should only be called when we expect sub-exp to reduce.
    (let-values ([(sub-exp-result inner-rule) (step sub-exp env)])
      (return-change
       (change-func sub-exp-result)
       (cons outer-rule inner-rule))))
  (match exp
    ;; VALUES
    [(? value?)
     (return-no-change)]
    ;; SYMBOLS
    [(? symbol?)
     ;; Attempt to look up symbols in the environment. If no binding exists,
     ;; it's an error. There is always something to reduce here.
     (let ([lookup (env-lookup env exp)])
       (if lookup
           (return-change lookup
                          'E-Substitute)
           (return-change (err-value (format "free variable: ~a" exp))
                          'Err-Substitute)))]
    ;; SUPERPOSITIONS
    [(superposition lhs rhs)
     (match (cons lhs rhs)
       [(cons (? err-value?) (? err-value?))
        ;; Reduction results in an error either way.
        (return-change
         (err-value "erroneous superposition")
         'Err-Superposition)]
       [(cons (? err-value?) _)
        (return-change rhs 'E-SuperpositionErrorReduce1)]
       [(cons _ (? err-value?))
        (return-change lhs 'E-SuperpositionErrorReduce2)]
       [(cons (? value?) _)
        (recur-step
         rhs
         (λ (new-rhs)
           (if (value? new-rhs)
               (superposition-value lhs new-rhs)
               (superposition lhs new-rhs)))
         'E-SuperpositionReduce2)]
       [_
        (recur-step
         lhs
         (λ (new-lhs) (superposition new-lhs rhs))
         'E-SuperpositionReduce1)])]
    ;; FUNCTIONS
    [`(,(or 'λ 'λ ) (,formals ...) ,body)
     (match formals
       [(list)
        #f])]))  ;; FIXME
