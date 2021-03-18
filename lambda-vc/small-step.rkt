#lang racket

(provide interp)

(require "env.rkt"
         "utility.rkt"
         "values.rkt")

;; Fully interpret the input expression. The result is either the resulting
;; reduced value on its own, or else a pair of the reduced value and the list of
;; rules that led to its reduction (if `#:with-rules` is given a non-false
;; value). By default, the prelude environment is used, but an alternate
;; environment can be specified via `#:in-env`.
(define (interp exp
                #:in-env [env prelude-env]
                #:with-rules [with-rules? #f])
  (let-values ([(result-exp rules) (interp-with-rules exp env)])
    (if with-rules?
        (cons result-exp rules)
        result-exp)))

;; Fully interpret the input expression within the given environment, returning
;; both the resulting reduced value as well as the list of rules that led to its
;; reduction.
(define (interp-with-rules exp env)
  (define (interp exp env rules rule-no)
    (let*-values ([(new-exp new-env new-rules) (step exp env rule-no)]
                  [(updated-rules) (append new-rules rules)]
                  [(last-rule-no) (car (first updated-rules))])
      (if (empty? new-rules)
          ;; No step was taken.
          (values new-exp updated-rules)
          ;; A step was taken.
          (interp new-exp new-env updated-rules (add1 last-rule-no)))))
  (interp exp env (list (list 0 'E-Original `,exp)) 1))

;; Attempts to perform a single-step reduction of `exp`. Returns a triple:
;;
;; 1. The returned expression.
;; 2. The returned environment.
;; 3. `#f` if the returned expression is identical to the input expression
;;    (i.e., if no reduction occurred). Otherwise, returns a list containing an
;;    inner list consisting of an integer, the symbol naming the rule
;;    responsible for the reduction, and a string form of the expression
;;    resulting from the reduction. The integer represents the rule's
;;    application number in the interpretation, relative to the given
;;    application number.
;;
;; NOTE: If multiple rules are applied in one step, the list returned in the
;;       third position of the triple will contain a list for each of them.
;;
;; NOTE: The fact that multiple steps can be taken in a single step is perhaps
;;       odd. This could be mitigated by using continuations or evaluation
;;       contexts, or else by losing some information about rule application.
;;       However, I chose to take this path as it seemed simplest for my needs.
(define (step exp env [rule-no 1])
  (define (return-change new-exp new-env rule)
    (values new-exp new-env (if (list? rule)
                                rule
                                (list (list rule-no rule `,exp '--> `,new-exp))
                                #;(list (list rule-no rule (format "~v --> ~v" exp new-exp))))))
  (define (return-no-change)
    (values exp env '()))
  (define (recur-step sub-exp sub-env change-func outer-rule)
    ;; This should only be called when we expect sub-exp to reduce.
    (let-values ([(new-exp new-env inner-rule) (step sub-exp sub-env rule-no)])
      (return-change
       (change-func new-exp)
       new-env
       (cons (list (add1 rule-no)
                   outer-rule
                   `,exp
                   '-->
                   `,new-exp) inner-rule))))
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
                          env
                          'E-Substitute)
           (return-change (err (format "free variable: ~a" exp))
                          env
                          'Err-Substitute)))]
    ;; SUPERPOSITIONS
    [`(,(or 'σ 'sigma) ,lhs ,rhs)
     (match (cons lhs rhs)
       [(cons (? err?) (? err?))
        ;; Reduction results in an error either way.
        (return-change
         env
         ;; TODO: Maybe include the inner errors' messages?
         (err "erroneous superposition")
         'Err-Superposition)]
       [(cons (? err?) _)
        (return-change rhs
                       env
                       'E-SuperpositionErrorReduce1)]
       [(cons _ (? err?))
        (return-change lhs
                       env
                       'E-SuperpositionErrorReduce2)]
       [(cons (? value?) (? value?))
        (return-change (superposition lhs rhs)
                       env
                       'V-Superposition)]
       [(cons (? value?) _)
        (recur-step rhs
                    env
                    (λ (new-rhs)
                      `(σ ,lhs ,new-rhs))
                    'E-SuperpositionReduce2)]
       [_
        (recur-step lhs
                    env
                    (λ (new-lhs)
                      `(σ ,new-lhs ,rhs))
                    'E-SuperpositionReduce1)])]
    ;; FUNCTIONS
    [`(,(or 'λ 'lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; A nullary function.
        (return-change (closure env #f body)
                       env
                       'V-NullaryFunction)]
       [(list formal formals ...)
        ;; A 1+-argument function.
        (if (not (valid-formal? formal))
            ;; The first formal given is not valid.
            (if (eq? formal '...)
                ;; The first formal given is a variadic ellipsis, which is only
                ;; allowed as the final parameter of a function.
                (return-change (err "non-final variadic ellipsis")
                               env
                               'Err-NonFinalVariadicEllipsis)
                ;; The first formal given is something else.
                (return-change (err (format "invalid formal: ~a" formal))
                               env
                               'Err-InvalidFormalParameter))
            ;; Look at the structure of the remaining formals.
            (match formals
              [(list)
               ;; There are no additional formal parameters, so we have a unary
               ;; function.
               (return-change (closure env (param formal) body)
                              env
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
                                  env
                                  'Err-InvalidVariadicFunction)
                   ;; We can build a variadic function, so do it!
                   (return-change (variadic-closure env (param formal) body)
                                  env
                                  'V-VariadicFunction))]
              [_
               ;; We have a non-empty list of additional formal parameters. Now
               ;; we curry it!
               (return-change (closure env (param formal) `(λ ,formals ,body))
                              env
                              'E-Curry)]))])]
    ;; APPLICATIONS
    [`(,aplicée ,args ...)
     ;; aplicée, noun [French]: that which has been applied
     ;; (I couldn't think of a better name for it, since the semantics allow for
     ;; either functions or superpositions to be used here.)
     (cond
       [(not (value? aplicée))
        ;; The aplicée has to be reduced before anything else. This is because
        ;; the presence of FFI functions prohibits us from currying arguments in
        ;; FFI calls, so we have to fully reduce the aplicée to determine
        ;; whether it's an ffi-closure that we should apply without currying
        ;; anything.
        (recur-step aplicée
                    env
                    (λ (new-aplicée)
                      `(,new-aplicée ,@args))
                    'E-AppReduceAplicée)]
       [(ffi-closure? aplicée)
        ;; Since we have an FFI function, we must apply it without currying the
        ;; arguments. (This is unfortunate, and the semantics would be nicer if
        ;; we could avoid this.)
        (let-values ([(values first-not-value not-values) (member-partition not-value? args)])
          (if (empty? not-values)
              ;; All arguments are values.
              ;;
              ;; Now we have to check if there are any superpositions in the
              ;; list of arguments. Application to superpositions causes a
              ;; fork in execution (itself represented as a superposition), so
              ;; we have to factor out each superpositional argument as a
              ;; superpositional wrapper to the FFI function call.
              (let-values ([(non-superpositions first-superposition unchecked-values)
                            (member-partition superposition? values)])
                (if (empty? unchecked-values)
                    ;; There are no remaining superpositions in the list of
                    ;; values, so we can actually call the function. We have to
                    ;; wrap this call in an error handler that will wrap the
                    ;; error in an err struct so that superpositional FFI
                    ;; application will work as expected (i.e., a single failing
                    ;; branch does not cause the entire program to fail).
                    (return-change (with-handlers ([(λ (e) #t)
                                                    (λ (e)
                                                      (err "failure during FFI function application"))])
                                     (apply (ffi-closure-func aplicée) values))
                                   env
                                   'E-AppFFI)
                    ;; We found a superposition in the list of values. We will
                    ;; factor it out to a top-level position surrounding the FFI
                    ;; function call, extracting its inner values as arguments
                    ;; to the wrapped call.
                    (match first-superposition
                      [(superposition lhs rhs)
                       (let ([inner-lhs `(,aplicée ,@non-superpositions ,lhs ,@unchecked-values)]
                             [inner-rhs `(,aplicée ,@non-superpositions ,rhs ,@unchecked-values)])
                         (return-change `(σ ,inner-lhs ,inner-rhs)
                                        env
                                        'E-AppReduceFFISuperposition))])))
              ;; We need to reduce the next argument.
              (recur-step first-not-value
                          env
                          (λ (new-value)
                            `(,aplicée ,@values ,new-value ,@not-values))
                          'E-AppReduceFFI)))]
       [else
        ;; Other forms of application look at the arguments next.
        (match args
          [(list)
           ;; Nullary application. We just extract the body.
           (match aplicée
             [(closure c-env #f c-body)
              (return-change c-body
                             c-env
                             'E-AppNullary)]
             [_
              (return-change (err (format "nullary application of non-nullary function: ~a" aplicée))
                             env
                             'Err-AppNullary)])]
          [(list arg)
           ;; Unary application.
           (cond
             [(not (value? arg))
              ;; Reduce the argument.
              (recur-step arg
                          env
                          (λ (new-arg)
                            `(,aplicée ,new-arg))
                          'E-AppReduceArgument)]
             [(superposition? arg)
              ;; Superpositional arguments are distributed.
              (match arg
                [(superposition lhs rhs)
                 (return-change `(σ (,aplicée ,lhs) (,aplicée ,rhs))
                                env
                                'E-AppReduceSuperposition)])]
             [else
              ;; Actually apply the thing! (Hopefully...)
              (match aplicée
                [(variadic-closure vc-env (param vc-formal-name) vc-body)
                 (match vc-env
                   [`((,last-formal . ,_) ,old-env ...)
                    (return-change `(σ ,vc-body ,(variadic-closure (extend-env last-formal vc-body old-env)
                                                                   (param vc-formal-name)
                                                                   vc-body))
                                   env
                                   'E-AppVariadic)])]
                [(closure c-env (param c-formal-name) c-body)
                 (return-change c-body
                                (extend-env c-formal-name arg c-env)
                                'E-AppUnary)]
                [(superposition lhs rhs)
                 (return-change `(σ (,lhs ,arg) (,rhs ,arg))
                                env
                                'E-AppSuperposition)]
                [_
                 (return-change (err (format "invalid aplicée: ~a" aplicée))
                                env
                                'Err-App)])])]
          [(list arg args ...)
           ;; Multary application in need of uncurrying.
           (return-change `((,aplicée ,arg) ,@args)
                          env
                          'E-AppUncurry)])])]
    ;; INVALID
    [_
     (return-change (err (format "invalid input: ~a" exp))
                    env
                    'Err-InvalidInput)]))
