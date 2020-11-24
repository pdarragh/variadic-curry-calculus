#lang racket
;; A modified lambda calculus with superpositions.

(provide (all-defined-out))

(struct param (name) #:transparent)
(struct clo (formal body env) #:transparent)
(struct clo-v clo () #:transparent)
(struct supos (env terms) #:transparent)
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

(define (interp exp env)
  (match exp
    [(? symbol?)
     (or (env-lookup env exp)
         `,exp)]  ;; NOTE: This could also be made to produce an .
    [(? value?)
     exp]
    [`(,(or `λ `lambda) (,formals ...) ,body)
     (match formals
       [(list)
        ;; nullary function
        (clo #f body env)]
       [(list formal)
        ;; unary function
        (clo (param formal) body env)]
       [(list formal '...)
        ;; variadic function
        (clo-v (param formal) body env)]
       [(cons formal formals)
        ;; auto-curried function
        (clo (param formal) `(λ ,formals ,body) env)])]
    [`(,(or `σ `sigma) ,terms ...)
     (displayln (format "σ -- terms: ~a" terms))
     (let ([interp-terms (map (λ (t) (interp t env))
                              terms)])
       (supos env interp-terms))]
    [`(,func ,@(list arg args ..1))
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
    [`(,func ,args ...)
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
         [(struct clo (#f c-body c-env))
          ;; function is nullary
          (match args
            [(list)
             ;; nullary application
             (interp c-body c-env)]
            [_
             ;; non-nullary application of nullary function
             (err (format "cannot apply nullary function with argument(s): ~a" args))])]
         [(struct clo ((struct param (c-param-name)) c-body c-env))
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
                    (supos env (list interp-body (clo-v (param formal) (list interp-body formal)))))]  ;; TODO: check environment.
                 [else
                  ;; application of a normal function
                  interp-body]))])]
         [(struct supos (s-env (list s-t1 s-t2)))
          ;; application of superposition
          (let ([t1-result (interp s-t1 s-env)]
                [t2-result (interp s-t2 s-env)])
            (match (cons t1-result t2-result)
              [(cons (struct err _) (struct err _))
               (err "erroneous superposition")]
              [(cons t1-result (struct err _))
               t1-result]
              [(cons t2-result (struct err _))
               t2-result]
              [else
               (supos env (list t1-result t2-result))]))]
         [_
          (err (format "not a function: ~a" interp-func))]))]
    [_
     (err (format "invalid input: ~a" exp))]))
