#lang racket

(provide (all-defined-out))

(require "env.rkt"
         "utility.rkt"
         "values.rkt")

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
     ;; The input expression is already fully reduced.
     (return-no-change)]
    ;; SYMBOLS
    [(? symbol?)
     ;; Attempt to look up symbols in the environment. If no binding exists,
     ;; it's an error. There is always something to reduce here.
     (let ([lookup (env-lookup env exp)])
       (if lookup
           (return-change lookup
                          'E-Substitute)
           (return-change (err (format "free variable: ~a" exp))
                          'Err-Substitute)))]
    ;; SUPERPOSITIONS
    [`(,(or 'σ 'sigma) ,lhs ,rhs)
     (match (cons lhs rhs)
       [(cons (? err?) (? err?))
        ;; Reduction results in an error either way.
        (return-change
         ;; TODO: Maybe include the inner errors' messages?
         (err "erroneous superposition")
         'Err-Superposition)]
       [(cons (? err?) _)
        (return-change rhs
                       'E-SuperpositionErrorReduce1)]
       [(cons _ (? err?))
        (return-change lhs
                       'E-SuperpositionErrorReduce2)]
       [(cons (? value?) (? value?))
        (return-change (superposition lhs rhs)
                       'V-Superposition)]
       [(cons (? value?) _)
        (recur-step rhs
                    (λ (new-rhs)
                      `(σ ,lhs ,new-rhs))
                    'E-SuperpositionReduce2)]
       [_
        (recur-step lhs
                    (λ (new-lhs)
                      `(σ ,new-lhs ,rhs))
                    'E-SuperpositionReduce1)])]
    ;; FUNCTIONS
    [`(,(or 'λ 'lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; A nullary function.
        (return-change (closure env #f body)
                       'V-NullaryFunction)]
       [(list formal formals ...)
        ;; A 1+-argument function.
        (if (not (valid-formal? formal))
            ;; The first formal given is not valid.
            (if (eq? formal '...)
                ;; The first formal given is a variadic ellipsis, which is only
                ;; allowed as the final parameter of a function.
                (return-change (err "non-final variadic ellipsis")
                               'Err-NonFinalVariadicEllipsis)
                ;; The first formal given is something else.
                (return-change (err (format "invalid formal: ~a" formal))
                               'Err-InvalidFormalParameter))
            ;; Look at the structure of the remaining formals.
            (match formals
              [(list)
               ;; There are no additional formal parameters, so we have a unary
               ;; function.
               (return-change (closure env (param formal) body)
                              'V-UnaryFunction)]
              [(list '...)
               ;; Variadic functions are defined by supplying a single formal
               ;; parameter with a trailing ellipsis.
               ;;
               ;; We only allow variadic functions to be defined within the
               ;; body of another function, because the outer function's formal
               ;; parameter will serve as the point at which to inject the
               ;; superpositional return value in subsequent applications of
               ;; the variadic function.
               ;;
               ;; TODO: At the moment, this check involves looking at the
               ;;       environment to determine if it's either empty or has an
               ;;       FFI function in the most-recently-bound position. This
               ;;       is fragile, and an alternative encoding of environments
               ;;       that separates the user's environment from the prelude
               ;;       would be preferred.
               (if (or (env-empty? env)
                       (ffi-closure? (cdar env)))
                   ;; The environment doesn't support a variadic function.
                   (return-change (err "variadic functions may only be defined within the body of another function")
                                  'Err-InvalidVariadicFunction)
                   ;; We can build a variadic function, so do it!
                   (return-change (variadic-closure env (param formal) body)
                                  'V-VariadicFunction))]
              [_
               ;; We have a non-empty list of additional formal parameters. Now
               ;; we curry it!
               (return-change (closure env (param formal) `(λ ,formals ,body))
                              'E-Curry)]))])]
    ;; APPLICATIONS
    [`(,aplicee ,args ...)
     (if (ffi-closure? aplicee)
         #f  ;; FIXME
         (match args
           [(list)
            ;; Nullary application.
            #f]  ;; FIXME
           [(list arg)
            ;; Unary application.
            #f]  ;; FIXME
           [(list arg args ...)
            ;; Multary application in need of uncurrying.
            (return-change `((,aplicee ,arg) ,@args)
                           'E-AppUncurry)]))]
    ;; INVALID
    [_
     (return-change (err (format "invalid input: ~a" exp))
                    'Err-InvalidInput)]))
