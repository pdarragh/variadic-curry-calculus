#lang racket
;; A modified lambda calculus with both automatic currying and variadic
;; functions.
;;
;; This is made possible by the use of superpositions, where indeterminate
;; results are considered non-deterministic and choices are delayed until later
;; application of such a result.

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structs
;;

;; Parameters are kept in a struct so we can distinguish nullary closures.
(struct param (name))
;; A closure is a tuple of:
;;   - The active environment when the closure was formed.
;;   - A formal parameter name.
;;   - A body yet to be interpreted.
;; The formal parameter can be omitted (creating a nullary function) by
;; supplying a #f value.
(struct closure (env formal body)
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (match c
       [(closure _ #f c-body)
        (write-string (format "(λ () ~a)" c-body)
                      port)]
       [(closure _ (param c-formal-name) c-body)
        (write-string (format "(λ (~a) ~a)" c-formal-name c-body)
                      port)]))])
;; Variadic closures are identical in composition to normal closures, but are
;; handled differently by the interpreter and are printed differently.
(struct variadic-closure closure ()
  #:methods gen:custom-write
  [(define (write-proc vc port mode)
     (match vc
       [(variadic-closure _ (param vc-formal-name) vc-body)
        (write-string (format "(λ (~a ...) ~a)" vc-formal-name vc-body)
                      port)]))])
;; Foreign Function Interface closures are not actually closures, but are
;; instead references to existing Racket functions.
(struct ffi-closure (func))
;; Superpositions encode two simultaneously valid values. These are used for
;; handling the semantics of variadic functions in an auto-curried world.
(struct superposition (last-result next-variadic-closure)
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (match s
       [(superposition r c)
        (write-string (format "(σ ~a ~a)" r c)
                      port)]))])
;; Error messages are handled in a struct so superposition collapsing can avoid
;; installing error handlers.
(struct err (message) #:transparent)

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

;; We provide a simple prelude to support basic arithmetic by making external
;; calls to Racket.
(define prelude
  (foldl (λ (pair env)
           (cons pair env))
         mt-env
         (list
          (cons '+ (ffi-closure +))
          (cons '- (ffi-closure -))
          (cons '* (ffi-closure *))
          (cons '/ (ffi-closure /)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpful Predicates
;;

(define (value? x)
  (or (integer? x)
      (closure? x)
      (ffi-closure? x)
      (superposition? x)))

(define (formal? formal)
  (and (symbol? formal)
       (regexp-match #px"^[[:alpha:]](-[[:word:]])*$"
                     (symbol->string formal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interpreter
;;

(define (interp exp env)
  (match exp
    [(? symbol?)
     (or (env-lookup env exp)
         `,exp)]  ;; NOTE: This could also be made to produce an error.
    [(? value?)
     exp]
    [(? err?)
     exp]
    [`(,(or `λ `lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; nullary function
        (closure env #f body)]
       [(list formal formals ...)
        ;; one or more formal parameters
        ;; validate the first one
        (if (not (formal? formal))
            (err (format "invalid formal parameter name: ~a" formal))
            (match formals
              [(list)
               ;; unary function
               (closure env (param formal) body)]
              [(list '...)
               ;; variadic function; check the environment contains a user-bound variable
               (if (or (env-empty? env)
                       (ffi-closure? (cdar env)))
                   (err "variadic functions may only be defined within the body of another function")
                   (variadic-closure env (param formal) body))]
              [else
               ;; multary function; curry it!
               ;; NOTE: Should the lambda produced here be interpreted first?
               (closure env (param formal) `(λ ,formals ,body))]))])]
    [`(,func ,args ...)
     (let ([interp-func (interp func env)])
       (cond
         [(ffi-closure? interp-func)
          ;; FFI application has to intercept the currying because Racket is not curried.
          (let ([interp-args (map (λ (arg) (interp arg env))
                                  args)])
            (eval `(,(ffi-closure-func interp-func) ,@interp-args)))]
         [(< 1 (length args))
          ;; multary application
          (let ([interp-first-app (interp `(,func ,(first args)) env)])
            (interp `(,interp-first-app ,@(rest args)) env))]
         [else
          ;; nullary or unary application
          (match interp-func
            [(closure c-env c-param c-body)
             (if (eq? #f c-param)
                 ;; function is nullary
                 (if (not (empty? args))
                     ;; non-nullary application of nullary function
                     (err (format "cannot apply nullary function with argument(s): ~a" args))
                     ;; nullary application
                     (interp c-body c-env))
                 ;; function is either unary or variadic
                 (if (empty? args)
                     ;; nullary application of non-nullary function
                     (err "non-nullary function applied without arguments")
                     ;; unary application
                     (let* ([interp-arg (interp (first args) env)]
                            [c-formal-name (param-name c-param)]
                            [new-env (extend-env c-formal-name interp-arg c-env)]
                            [interp-body (interp c-body new-env)])
                       (if (variadic-closure? interp-func)
                           ;; application of a variadic function
                           (match c-env
                             ;; The environment of a variadic function is non-empty by construction.
                             [`((,last-formal . ,last-val) ,old-env ...)
                              ;; FIXME (λ (last-formal) (λ (v-formal ...) v-body))
                              (superposition interp-body
                                     (variadic-closure (extend-env last-formal interp-body old-env)
                                            c-param
                                            c-body))])
                           ;; application of a normal function
                           interp-body))))]
            [(superposition last-result next-variadic-closure)
             ;; TODO: Check the environments used for interp sub-calls are correct.
             (let ([interp-last-result-app (interp `(,last-result ,@args) env)]
                   [interp-next-variadic-closure-app (interp `(,next-variadic-closure ,@args) env)])
               (match (cons interp-last-result-app interp-next-variadic-closure-app)
                 [(cons (? err?) (? err?))
                  ;; both branches resulted in an error
                  (err "erroneous superposition")]
                 [(or (cons (? err?) result)
                      (cons result (? err?)))
                  ;; there was exactly one result
                  result]
                 [else
                  ;; Both results are valid, so we remain in a superposition.
                  (superposition interp-last-result-app
                                 interp-next-variadic-closure-app)]))]
            [(err msg)
             ;; pass the error along
             interp-func]
            [else
             ;; encountered something we... did not expect
             (err (format "not a function: ~a" interp-func))])]))]
    [_
     (err (format "invalid input: ~a" exp))]))
