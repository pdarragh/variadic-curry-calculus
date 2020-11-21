#lang racket
;; A modified lambda calculus with superpositions.

(provide mt-env interp)

(struct param (name) #:transparent)
(struct clo (formal body env) #:transparent)
(struct clo-v clo () #:transparent)
(struct supos (value func) #:transparent)

(define mt-env empty)
(define (extend-env env name value)
  (cons (cons name value) env))
(define (env-lookup env name)
  (match env
    [(list) #f]
    [(cons (cons l-name l-value) rest-env)
     (if (equal? name l-name)
         l-value
         (env-lookup rest-env name))]))

(define (value? x)
  (or (integer? x)
      (clo? x)
      (clo-v? x)
      (supos? x)))

(define (interp exp env)
  (match exp
    [(? symbol?)
     (or (env-lookup env exp)
         `,exp)]  ;; NOTE: This could also be made to produce an error.
    [(? value?)
     exp]
    [(list (or 'λ 'lambda) (list formals ...) body)  ;; NOTE: Can be rewritten as: `(,(or `λ `lambda) (,formals ...) ,body)
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
    [`(,func ,@(list arg args ..1))
     ;; binary+ application
     (let ([interp-app (interp (list func arg) env)])
       (interp `(,interp-app ,@args) env))]
    [(list func args ...)  ;; NOTE: Can be rewritten as: `(,func ,args ...)
     ;; nullary or unary application
     (let ([interp-func (interp func env)])
       (match interp-func
         [(struct clo (#f c-body c-env))
          ;; function is nullary
          (match args
            [(list)
             ;; nullary application
             (interp c-body c-env)]
            [_
             ;; non-nullary application of nullary function
             (error 'interp (format "cannot apply nullary function with argument(s): ~a" args))])]
         [(or (struct clo ((struct param (c-param-name)) c-body c-env))
              (struct supos (_ (struct clo ((struct param (c-param-name)) c-body c-env)))))
          ;; function is unary
          (match args
            [(list arg)
             ;; unary application
             (let* ([interp-arg (interp arg env)]
                    [new-env (extend-env c-env c-param-name interp-arg)]
                    [interp-body (interp c-body new-env)])
               (if (not (clo-v? interp-func))
                   interp-body
                   ;; ((λ (p1 ...) body) a1)  =>  (σ body[a1/p1] (λ (p1 ...) (body[a1/p1] p1)))
                   (let ([formal (gensym "formal")])
                     (supos interp-body (clo-v (param formal) (list interp-body formal) new-env)))))]
            [_
             ;; nullary application of unary function
             (error 'interp (format "cannot apply function without parameter: ~a" c-param-name))])]
         [_
          (error 'interp (format "not a function: ~a" interp-func))]))]
    [_
     (error 'interp (format "invalid input: ~a" exp))]))
