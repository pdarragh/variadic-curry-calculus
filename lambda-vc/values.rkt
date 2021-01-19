#lang racket

(provide (struct-out err)
         (struct-out param)
         (struct-out closure)
         (struct-out variadic-closure)
         (struct-out ffi-closure)
         (struct-out superposition)
         (rename-out [value-pred? value?]
                     [not-value-pred? not-value?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VALUE
;;

;; All "values" are sub-types of the value struct. This way, we can use the
;; predicate `value?` to determine all values.
(struct value ())

;; We include integers in the set of values.
(define (value-pred? x)
  (or (integer? x)
      (value? x)))

;; A convenience function for inverted predicates.
(define (not-value-pred? x)
  (not (value-pred? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ERROR
;;

;; Error messages are wrapped in structs so superposition collapsing can avoid
;; installing error handlers.
(struct err value (message) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CLOSURE
;;

;; Closure parameters are wrapped in a struct so we can distinguish nullary
;; closures.
(struct param (name))

;; A closure is a tuple of:
;;   - The active environment when the closure was formed.
;;   - A formal parameter name.
;;   - A body yet to be interpreted.
;; The formal parameter can be omitted (creating a nullary function) by
;; supplying a #f value. Note that a closure has at most one formal parameter.
(struct closure value (env formal body)
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
(struct ffi-closure value (func name)
  #:methods gen:custom-write
  [(define (write-proc fc port mode)
     (match fc
       [(ffi-closure _ name)
        (write-string (format "ϕ(~a)" name)
                      port)]))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SUPERPOSITION
;;

;; Superpositions are just two grouped inner expressions.
(struct superposition value (lhs rhs)
  #:methods gen:custom-write
  [(define (write-proc s port mode)
     (match s
       [(superposition lv rv)
        (write-string (format "(σ ~a ~a)" lv rv)
                      port)]))])
