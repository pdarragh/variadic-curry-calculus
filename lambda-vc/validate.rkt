#lang racket

(provide (all-defined-out))

(define (valid-formal? formal)
  (and (symbol? formal)
       (regexp-match #px"^[[:alpha:]](-[[:word:]])*$"
                     (symbol->string formal))))

(define (syntax-validate exp)
  (match exp
    [(? integer?) #t]
    [(? valid-formal?) #t]
    [`(,(or 'λ 'lambda) (,formals ...) ,body)
     (and (andmap (λ (f) (or (valid-formal? f)
                             (eq? f '...)))
                  formals)
          (andmap (λ (f) (not (eq? f '...)))
                  (drop-right formals 1))
          (syntax-validate body))]
    [`(,func ,args ...)
     (and (syntax-validate func)
          (andmap syntax-validate
                  args))]
    [_ #f]))
