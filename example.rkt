#lang racket

(require lambda-vc)

(define plus1 `(λ (x) (+ 1 x)))
(define vcompose `(λ (a b ...) (λ (x) (a (b x)))))
(interp `(,vcompose ,plus1 ,plus1 5))        ;; works fine: (σ 7 ....)
(interp `(,vcompose ,plus1 ,plus1 ,plus1 5)) ;; fails

(interp '(+ 0 ((λ (a b ...) (+ a b)) 1 2)))
