#lang racket
;; A modified lambda calculus with both automatic currying and variadic
;; functions.
;;
;; This is made possible by the use of superpositions, where indeterminate
;; results are considered non-deterministic and choices are delayed until later
;; application of such a result.

(provide (rename-out [exposed-interp interp]))

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
;; Helpful Functions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interpreter
;;

;; The function we expose to the user only takes an expression as argument, and
;; automatically interprets that expression in the context of the prelude. We
;; also provide nicer error messages, because we're cool like that.
(define (exposed-interp exp)
  (let ([result (interp exp prelude)])
    (if (err? result)
        (displayln (format "ERROR: ~a" (err-message result)))
        result)))

;; This is the actual interpreter implementation.
(define (interp exp env)
  (match exp
    ;; Superpositions must be checked first.
    [(superposition l-val r-val)
     ;; Interpretation of a superposition looks to see if either branch can be
     ;; collapsed to produce a single (non-superpositional) value.
     (match (cons l-val r-val)
       ;; Did both branches produce errors?
       [(cons (? err?) (? err?))
        ;; Yes, so we produce a new error.
        (err "erroneous superposition")]
       ;; Did either branch succeed?
       [(or (cons (? err?) result)
            (cons result (? err?)))
        ;; Return the successful result.
        result]
       ;; Otherwise, both branches succeed, so we remain in a superposition.
       [else exp])]
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
               ;; If there are no additional formal parameters, we have a unary
               ;; (one-argument) function.
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
     (define (intercept-errors-interp body)
       (with-handlers ([(λ (e) #t)
                        (λ (e)
                          (err "failure during superposition sub-interpretation"))])
         (interp body env)))
     ;; Applications are determined by the shape of the term on the left.
     (let ([interp-func (interp func env)])
       (cond
         ;; Foreign function application.
         [(ffi-closure? interp-func)
          ;; FFI application has to intercept the currying because Racket is not
          ;; curried. I don't know of a way around this.
          ;;
          ;; Unfortunately, this makes handling application to superpositions
          ;; tricky because we now need to peek through the entire list to find
          ;; if any argument is a superposition whose interpretation should be
          ;; forked. We do this by interpreting the arguments in-order and
          ;; simply stopping when we come to the first superposition, relying on
          ;; recursive calls to handle the subsequent arguments as needed.
          ;;
          ;; This involves some needless recursive calls, but these will mostly
          ;; resolve quickly since the arguments we keep re-interpreting should
          ;; be fundamental values after the first pass over the argument list.
          (let* ([interp-args (map (λ (arg) (interp arg env))
                                   args)]
                 [first-superposition (findf+index superposition? interp-args)])
            (if first-superposition
                ;; We have a superposition, so interpretation must fork.
                (match (car first-superposition)
                  [(superposition last-result next-variadic-closure)
                   (let*-values ([(good-args other-args) (split-at
                                                          interp-args
                                                          (cdr first-superposition))]
                                 [(interp-arg-func) (λ (arg)
                                                      (intercept-errors-interp
                                                       `(,interp-func
                                                         ,@good-args
                                                         ,(interp arg env)
                                                         ,@(rest other-args))))]
                                 ;; Interp the first value.
                                 [(interp-last-result-app) (interp-arg-func last-result)]
                                 ;; Now interp the second value.
                                 [(interp-next-variadic-closure-app) (interp-arg-func next-variadic-closure)])
                     ;; We push both interpretation results into the
                     ;; superposition.
                     (interp (superposition interp-last-result-app
                                            interp-next-variadic-closure-app)
                             env))])
                ;; There is no superposition in the arguments, so make the call.
                (apply (ffi-closure-func interp-func) interp-args)))]
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
                     ;; We now interpret the argument. If it is a superposition,
                     ;; we fork interpretation and merge later. This allows us
                     ;; to avoid exposing any observational literals into the
                     ;; language while still keeping superpositions useful in
                     ;; many situations.
                     (let ([interp-arg (interp (first args) env)])
                       (match interp-arg
                         [(superposition last-result next-variadic-closure)
                          ;; With a superposition, we fork interpretation and
                          ;; recurse (to clean up error branches).
                          (interp (superposition (interp `(,interp-func ,last-result) env)
                                                 (interp `(,interp-func ,next-variadic-closure) env))
                                  env)]
                         [else
                          ;; Just a normal argument for application.
                          (let* ([c-formal-name (param-name c-param)]
                                 [new-env (extend-env c-formal-name interp-arg c-env)]
                                 [interp-body (interp c-body new-env)])
                            (if (not (variadic-closure? interp-func))
                                ;; We're applying a normal function, so just
                                ;; return the interpreted result.
                                interp-body
                                ;; If we're applying a variadic function, we
                                ;; need to produce a superpositional result.
                                ;; This is because we don't know whether this is
                                ;; the last argument to apply to this function.
                                ;;
                                ;; To construct a subsequent variadic function
                                ;; to put in the superposition, we retrieve the
                                ;; most-recently-bound formal parameter name and
                                ;; construct a new variadic function whose
                                ;; environment has this name bound to the result
                                ;; of interpreting the body that we just got
                                ;; back.
                                (match c-env
                                  ;; The environment of a variadic function is
                                  ;; non-empty by construction, so destructure.
                                  [`((,last-formal . ,last-val) ,old-env ...)
                                   ;; The result is a superposition of the
                                   ;; interpreted body (as though this
                                   ;; application were the last) and a new
                                   ;; variadic function that can be used to
                                   ;; supply additional arguments to the same
                                   ;; computation.
                                   (interp (superposition interp-body
                                                          (variadic-closure (extend-env last-formal interp-body old-env)
                                                                            c-param
                                                                            c-body))
                                           env)])))]))))]
            ;; Superposition application.
            [(superposition last-result next-variadic-closure)
             ;; We apply the arguments to both the result and the variadic
             ;; function stored in the superposition (because we don't know
             ;; which path is "correct").
             ;;
             ;; While performing these interpretations, capture any errors so
             ;; the superposition can properly collapse as needed.
             (let ([interp-last-result-app (intercept-errors-interp `(,last-result ,@args))]
                   [interp-next-variadic-closure-app (intercept-errors-interp `(,next-variadic-closure ,@args))])
               ;; We immediately interp on the constructed superposition. This
               ;; will collapse any error branches.
               (interp (superposition interp-last-result-app
                                      interp-next-variadic-closure-app)
                       env))]
            ;; Application of something else?
            [else
             ;; This is undefined by the semantics of lambda-vc.
             (err (format "not a function: ~a" interp-func))])]))]
    ;; Some other form?
    [_
     ;; No. Bad. Don't do this.
     (err (format "invalid input: ~a" exp))]))
