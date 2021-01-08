(require "lambda-vc.rkt")

(provide (all-defined-out))

(struct closure ...)
(struct closure-value ...)

(struct function (formals body) #:transparent)

(define (value? x)
  (or (integer? x)
      (closure-value? x)
      ...))

(define (valid-formal? formal)
  (and (symbol? formal)
       (regexp-match #px"^[[:alpha:]](-[[:word:]])*$"
                     (symbol->string formal))))

(define (parse exp)
  (match exp
    [(? value?) exp]
    [(? symbol?)
     (if (valid-formal? exp)
         ...
         ...)]
    [`(,(or 'λ 'lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ...]
       [(list other-formals '...)
        ...]
       [else
        ...])
     (function (map parse formals) (parse body))]))

(define (small-step exp env)
  (match exp
    [(? closure?)
     (? )]
    [(? symbol?)
     ;; RULE: E-Substitute
     (or (env-lookup env exp)
         (err (format "interpretation of free variable: ~a" exp)))]
    [`(,(or `λ `lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; A nullary (zero-argument) function.
        (closure env #f body)]
       [(list formal formals ...)
        (if (not (valid-formal? formal))
            (err (format "invalid formal parameter name: ~a" formal))
            (match formals
              [(list)
               ;; A unary (one-argument) function.
               (closure env (param formal) body)]
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
                   (err "variadic functions may only be defined within the body of another function")
                   ;; We can build a variadic function, so do it!
                   (variadic-closure env (param formal) body))]
              [else
               ;; RULE: E-Curry
               (closure env (param formal) `(λ ,formals ,body))]))])]))
