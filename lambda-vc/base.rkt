#lang racket
;; A modified lambda calculus with superpositions.

(provide (all-defined-out))

(struct param (name))
(struct clo (env formal body)
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (match c
       [(clo _ #f c-body)
        (write-string (format "(λ () ~a)" c-body)
                      port)]
       [(clo _ (param c-param-name) c-body)
        (write-string (format "(λ (~a) ~a)" c-param-name c-body)
                      port)]))])
(struct clo-v clo ()
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (write-string (format "(λ (~a ...) ~a)"
                           (param-name (clo-formal c))
                           (clo-body c))
                   port))])
(struct supos (last-result next-clo-v)
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (match s
       [(supos r c)
        (write-string (format "(σ ~a ~a)" r c)
                      port)]))])
(struct clo-ffi (func))
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
  (regexp-match #px"^[[:alpha:]](-[[:word:]])*$" s))

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
    [(? err?)
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
              [(list '...)
               ;; variadic function; check the environment contains a user-bound variable
               (if (or (env-empty? env)
                       (clo-ffi? (cdar env)))
                   (err "variadic functions may only be defined within the body of another function")
                   (clo-v env (param formal) body))]
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
            [(clo c-env c-param c-body)
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
                            [interp-body (interp c-body new-env)])
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
                           interp-body))))]
            [(supos last-result next-clo-v)
             ;; TODO: Check the environments used for interp sub-calls are correct.
             (let ([interp-last-result-app (interp `(,last-result ,@args) env)]
                   [interp-next-clo-v-app (interp `(,next-clo-v ,@args) env)])
               (match (cons interp-last-result-app interp-next-clo-v-app)
                 [(cons (? err?) (? err?))
                  ;; both branches resulted in an error
                  (err "erroneous superposition")]
                 [(or (cons (? err?) result)
                      (cons result (? err?)))
                  ;; there was exactly one result
                  result]
                 [else
                  ;; Both results are valid, so we remain in a superposition.
                  (supos interp-last-result-app
                         interp-next-clo-v-app)]))]
            [(err msg)
             ;; pass the error along
             interp-func]
            [else
             ;; encountered something we... did not expect
             (err (format "not a function: ~a" interp-func))])]))]
    [_
     (err (format "invalid input: ~a" exp))]))
