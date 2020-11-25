#lang racket
;; A modified lambda calculus with superpositions.

(provide (all-defined-out))

(struct param (name) #:transparent)
(struct clo (env formal body) #:transparent)
(struct clo-v clo () #:transparent)
(struct supos (last-result next-clo-v) #:transparent)
(struct clo-ffi (func) #:transparent)
(struct err (message) #:transparent)

(define mt-env empty)
(define (extend-env name value env)
  (cons (cons name value) env))
(define (env-lookup env name)
  (match env
    [(list) #f]
    [(cons (cons l-name l-value) rest-env)
     (if (equal? name l-name)
         l-value
         (env-lookup rest-env name))]))
(define env-empty? empty?)

(define prelude
  (foldl (λ (pair env)
           (extend-env (car pair)
                       (cdr pair)
                       env))
         mt-env
         (list
          (cons '+ (clo-ffi +))
          (cons '- (clo-ffi -))
          (cons '* (clo-ffi *))
          (cons '/ (clo-ffi /)))))

(define (value? x)
  (or (integer? x)
      (clo? x)
      (clo-v? x)
      (clo-ffi? x)
      (supos? x)))

(define (string-identifier? s)
  (regexp-match #rx"^[:alpha:](-[:word:])*$"))

(define (formal? formal)
  (and (symbol? formal)
       (string-identifier? (symbol->string formal))))

(define (interp exp env)
  (match exp
    [(? symbol?)
     (or (env-lookup env exp)
         `,exp)]  ;; NOTE: This could also be made to produce an error.
    [(? value?)
     exp]
    [`(,(or `λ `lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; nullary function
        (clo env #f body)]
       [(list formal formals ...)
        ;; one or more formal parameters
        ;; validate the first one
        (if (not (formal? formal))
            (err (format "invalid formal parameter name: ~a" formal))
            (match formals
              [(list)
               ;; unary function
               (clo env (param formal) body)]
              [(list formal '...)
               ;; variadic function; check the environment is not empty
               (if (env-empty? env)
                   (err "variadic functions may only be defined within the body of another function")
                   (clo-v (param formal) body env))]
              [else
               ;; multary function; curry it!
               ;; NOTE: Should the lambda produced here be interpreted first?
               (clo env (param formal) `(λ ,formals ,body))]))])]
    [`(,func ,args ...)
     (let ([interp-func (interp func env)])
       (cond
         [(clo-ffi? interp-func)
          ;; FFI application has to intercept the currying because Racket is not curried.
          (let ([interp-args (map (λ (arg) (interp arg env))
                                  args)])
            (eval `(,(clo-ffi-func interp-func) ,@interp-args)))]
         [(< 1 (length args))
          ;; multary application
          (let ([interp-first-app (interp `(,func ,(first args)) env)])
            (interp `(,interp-first-app ,@(rest args)) env))]
         [else
          ;; nullary or unary application
          (match interp-func
            [(struct clo (c-env c-param c-body))
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
                            [c-param-name (param-name c-param)]
                            [new-env (extend-env c-param-name interp-arg c-env)]
                            [interp-body (interp c-body new-env)]))
                     (if (clo-v? interp-func)
                         ;; application of a variadic function
                         (match c-env
                           ;; The environment of a variadic function is non-empty by construction.
                           [`((,last-formal . ,last-val) ,old-env ...)
                            ;; FIXME (λ (last-formal) (λ (v-formal ...) v-body))
                            (supos interp-body
                                   (clo-v (extend-env last-formal interp-body old-env)
                                          c-param
                                          c-body))])
                         ;; application of a normal function
                         interp-body)))]
            [(struct supos (last-result next-clo-v))
             (let* ([interp-last-result-app `(,last-result ,@args)]
                    [interp-next-clo-v-app `(,next-clo-v ,@args)]
                    [filtered-results (filter (λ (r) (not (err? r)))
                                              (list interp-last-result-app
                                                    interp-next-clo-v-app))])
               (match filtered-results
                 [(list)
                  ;; every branch resulted in an error
                  (err "erronneous superposition")]
                 [(list result)
                  ;; there was exactly one result
                  resuult]
                 [else
                  ;; TODO: check what kind of results we got.
                  (err "UNIMPLEMENTED")]))])]))]
    #;[`(,func ,@(list arg args ..1))
     (let ([interp-func (interp func env)])
       (cond
         [(clo-ffi? interp-func)
          ;; FFI application has to intercept the currying because Racket is not curried.
          (let ([interp-arg (interp arg env)]
                [interp-args (map (λ (arg) (interp arg env))
                                  args)])
            (eval `(,(clo-ffi-func interp-func) ,interp-arg ,@interp-args)))]
         [else
          ;; multary application
          (let ([interp-app (interp `(,func ,arg) env)])
            (interp `(,interp-app ,@args) env))]))]
    #;[`(,func ,args ...)
     ;; nullary or unary application
     (let ([interp-func (interp func env)])
       (match interp-func
         [(struct clo-ffi (ffi-func))
          (match args
            [(list)
             (eval `(,ffi-func))]
            [(list arg)
             (let ([interp-arg (interp arg env)])
               (eval `(,ffi-func ,arg)))])]
         [(struct clo (c-env #f c-body))
          ;; function is nullary
          (match args
            [(list)
             ;; nullary application
             (interp c-body c-env)]
            [_
             ;; non-nullary application of nullary function
             (err (format "cannot apply nullary function with argument(s): ~a" args))])]
         [(struct clo (c-env (struct param (c-param-name)) c-body))
          ;; function is unary
          (match args
            [(list arg)
             ;; unary application
             (let* ([interp-arg (interp arg env)]
                    [new-env (extend-env c-param-name interp-arg c-env)]
                    [interp-body (interp c-body new-env)])
               (cond
                 [(clo-v? interp-func)
                  ;; application of a variadic function
                  (let ([formal (gensym "formal")])
                    ;; TODO: Check that this environment is correct.
                    ;; TODO: Revise function return value (it isn't correct).
                    (supos env (list interp-body (clo-v (param formal) (list interp-body formal)))))]
                 [else
                  ;; application of a normal function
                  interp-body]))])]
         ;; add2 = (λ (x ...) (+ 2 x))
         ;;
         ;; XXX
         ;;  Okay, maybe I've got it.
         ;;  - variadic functions must take two arguments
         ;;  - use lists to store intermediate results
         ;;  - keep the lists hidden from users (no need for lists in user-space)
         ;;  - each application of the function replaces arg1 with the last value computed
         ;;  - now variadic functions work like folds
         ;;  - each value in the intermediate list is a valid "result" of the application
         ;;  - application of superposition extracts both the last element of the list and applies the function
         ;;
         [(struct supos (s-env (list terms ...)))
          (let* ([term-results (map (λ (term) (interp term s-env))
                                    terms)]
                 [filtered-term-results (filter (λ (result) (not (err? result)))
                                                term-results)])
            (match filtered-term-results
              [(list)
               ;; every branch resulted in an error
               (err "erroneous superposition")]
              [(list term)
               ;; there was exactly one result
               term]
              [else
               ;; multiple branches did not result in errors
               (supos env filtered-term-results)]))]
         [_
          (err (format "not a function: ~a" interp-func))]))]
    [_
     (err (format "invalid input: ~a" exp))]))
