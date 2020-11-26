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

;; In this lambda calculus, we admit the following fundamental values:
;;   - Integers.
;;   - Closures (normal, variadic, and FFI).
;;   - Superpositions.
(define (value? x)
  (or (integer? x)
      (closure? x)
      (ffi-closure? x)
      (superposition? x)))

;; User-defined functions must have formal parameter names that match the
;; regular expression given below. Rendered in English, this correlates to names
;; which begin with an alphabetical character and are then followed by zero or
;; more hyphens or 'word' characters, which include the alphabetical characters,
;; digits, and underscores.
(define (valid-formal? formal)
  (and (symbol? formal)
       (regexp-match #px"^[[:alpha:]](-[[:word:]])*$"
                     (symbol->string formal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interpreter
;;

(define (interp exp env)
  (match exp
    ;; Errors or values.
    [(or (? err?)
         (? value?))
     ;; Return it as-is.
     exp]
    ;; Symbols.
    [(? symbol?)
     ;; We attempt to look up symbols in the environment. If no binding exists,
     ;; it's an error.
     (or (env-lookup env exp)
         (err (format "free variable: ~a" exp)))]
    ;; Function declarations.
    [`(,(or `λ `lambda) (,formals ...) ,body)
     ;; Function declarations are differentiated by the given formal parameters.
     (match formals
       [(list)
        ;; A nullary (zero-argument) function.
        (closure env #f body)]
       [(list formal formals ...)
        ;; A 1+-argument function.
        ;; We have to ensure the first parameter name given is valid.
        (if (not (valid-formal? formal))
            ;; If the formal parameter is invalid, produce an error.
            (err (format "invalid formal parameter name: ~a" formal))
            ;; Otherwise, we look at the remaining formal parameters.
            (match formals
              [(list)
               ;; If there are no additional formal parameters, we have a
               ;; unary (one-argument) function.
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
               ;; We have a non-empty list of additional formal parameters. Now
               ;; we curry it!
               ;;
               ;; TODO: Should the lambda produced here be interpreted first?
               (closure env (param formal) `(λ ,formals ,body))]))])]
    ;; Function and superposition applications.
    [`(,func ,args ...)
     ;; Applications are determined by the shape of the term on the left.
     (let ([interp-func (interp func env)])
       (cond
         ;; Foreign function applications
         [(ffi-closure? interp-func)
          ;; FFI application has to intercept the currying because Racket is not
          ;; curried. I don't know of a way around this.
          (let ([interp-args (map (λ (arg) (interp arg env))
                                  args)])
            ;; Call out to Racket using `eval`.
            (eval `(,(ffi-closure-func interp-func) ,@interp-args)))]
         ;; Multary (application of 2 or more terms at once).
         [(< 1 (length args))
          ;; We rewrite the application as a staging of a single-argument
          ;; application surrounded by a multary application.
          (let ([interp-first-app (interp `(,func ,(first args)) env)])
            (interp `(,interp-first-app ,@(rest args)) env))]
         ;; Nullary or unary application.
         [else
          (match interp-func
            ;; Error application. (?)
            [(err msg)
             ;; Instead of creating a new error, let's just propagate this error
             ;; back to the top.
             interp-func]
            ;; Function application.
            [(closure c-env c-param c-body)
             (if (eq? #f c-param)
                 ;; The function is nullary, so we should not have arguments.
                 (if (not (empty? args))
                     ;; Uh-oh; somebody applied the function to something!
                     (err (format "cannot apply nullary function with argument(s): ~a" args))
                     ;; This is okay.
                     (interp c-body c-env))
                 ;; The function is either unary or variadic, both of which
                 ;; require an argument to apply.
                 (if (empty? args)
                     ;; No arguments were given!
                     (err "non-nullary function applied without arguments")
                     ;; This is a successful application pathway.
                     ;;
                     ;; We now interpret the argument, substitute its value into
                     ;; the environment, and then interpret the body of the
                     ;; function in that environment.
                     (let* ([interp-arg (interp (first args) env)]
                            [c-formal-name (param-name c-param)]
                            [new-env (extend-env c-formal-name interp-arg c-env)]
                            [interp-body (interp c-body new-env)])
                       (if (not (variadic-closure? interp-func))
                           ;; We're applying a normal function, so just return
                           ;; the interpreted result.
                           interp-body
                           ;; If we're applying a variadic function, we need to
                           ;; produce a superpositional result. This is because
                           ;; we don't know whether this is the last argument
                           ;; to apply to this function.
                           ;;
                           ;; To construct a subsequent variadic function to put
                           ;; in the superposition, we retrieve the most-
                           ;; recently-bound formal parameter name and construct
                           ;; a new variadic function whose environment has this
                           ;; name bound to the result of interpreting the body
                           ;; that we just got back.
                           (match c-env
                             ;; The environment of a variadic function is
                             ;; non-empty by construction, so destructure it.
                             [`((,last-formal . ,last-val) ,old-env ...)
                              ;; The result is a superposition of the
                              ;; interpreted body (as though this application
                              ;; were the last) and a new variadic function that
                              ;; can be used to supply additional arguments to
                              ;; the same computation.
                              (superposition interp-body
                                             (variadic-closure (extend-env last-formal interp-body old-env)
                                                               c-param
                                                               c-body))])))))]
            ;; Superposition application.
            [(superposition last-result next-variadic-closure)
             ;; We apply the arguments to both the result and the variadic
             ;; function stored in the superposition (because we don't know
             ;; which path is "correct").
             (let ([interp-last-result-app (interp `(,last-result ,@args) env)]
                   [interp-next-variadic-closure-app (interp `(,next-variadic-closure ,@args) env)])
               (match (cons interp-last-result-app interp-next-variadic-closure-app)
                 ;; Both branches error...
                 [(cons (? err?) (? err?))
                  ;; So we produce a new error.
                  (err "erroneous superposition")]
                 ;; One branch succeeds...
                 [(or (cons (? err?) result)
                      (cons result (? err?)))
                  ;; We return the successful result.
                  result]
                 ;; Both branches succeed...
                 [else
                  ;; We remain in a superposition.
                  ;;
                  ;; NOTE: We don't have to consider the implications of this
                  ;;       too hard. If the variadic function succeeded (which
                  ;;       it must have for us to be here), then the result of
                  ;;       that application is itself a superposition. Now we
                  ;;       have nested superpositions, which isn't phenomenal
                  ;;       but it is easier than extracting the results and
                  ;;       putting them into some kind of non-deterministic data
                  ;;       structure (like a list).
                  (superposition interp-last-result-app
                                 interp-next-variadic-closure-app)]))]
            ;; Application of something else?
            [else
             ;; This is undefined by the semantics of lambda-vc.
             (err (format "not a function: ~a" interp-func))])]))]
    ;; Some other form?
    [_
     ;; No. Bad. Don't do this.
     (err (format "invalid input: ~a" exp))]))
