#lang racket

(require scribble-math)

(provide (all-defined-out))

(use-mathjax)

(define (separate-text-by-pred text pred)
  (define (group->text-pair group group-matches-pred?)
    (cons group-matches-pred? (list->string (reverse group))))
  (define (separate-chars group group-matches-pred? groups chars)
    (match chars
      [(list)
       (reverse (cons (group->text-pair group group-matches-pred?)
                      groups))]
      [(list c rest-chars ...)
       (if (pred c)
           (if group-matches-pred?
               ;; Add c to existing matching group.
               (separate-chars (cons c group) #t groups rest-chars)
               ;; Start a new matching group.
               (separate-chars (list c)
                               #t
                               (cons (group->text-pair group group-matches-pred?)
                                     groups)
                               rest-chars))
           (if group-matches-pred?
               ;; Start a new non-matching group.
               (separate-chars (list c)
                               #f
                               (cons (group->text-pair group group-matches-pred?)
                                     groups)
                               rest-chars)
               ;; Add c to existing non-matching group.
               (separate-chars (cons c group) #f groups rest-chars)))]))
  (define-values (first-char chars)
    (let ([all-chars (string->list text)])
      (values (first all-chars)
              (rest all-chars))))
  (separate-chars (list first-char) (pred first-char) (list) chars))

(define (textsc text)
  (string-join
   #:before-first "{\\rm "
   (map
    (λ (text)
      (string-join
       (map (match-lambda [(cons sc? text)
                           (if sc?
                               (format "{\\small ~a}" (string-upcase text))
                               text)])
            (separate-text-by-pred
             text
             char-lower-case?))))
    (string-split text))
   #:after-last "}"))

(define (mk-term term)
  (λ ([sub #f])
    (let ([term-text (format "\\mathtt{\\mathbf{~a}}" term)])
      (if sub
          (format "{~a}_{~a}" term-text sub)
          term-text))))

(define t (mk-term "t"))
(define v (mk-term "v"))
(define x (mk-term "x"))

(define ($$rule-name name)
  ($$ (string-join
       (map textsc (string-split name "-"))
       (format "\\text{-}"))))

(define (split-on lst sep)
  (define (split-on sep acc rem)
    (match rem
      [(list)
       (values acc '())]
      [(list next rest-rem ...)
       (if (equal? next sep)
           (values acc rest-rem)
           (split-on sep (cons next acc) rest-rem))]))
  (let-values ([(acc rem) (split-on sep '() lst)])
    (values (reverse acc) rem)))

(define g-epsilon "\\epsilon")
(define g-lambda "\\lambda")
(define g-sigma "\\sigma")
(define g-Gamma "\\Gamma")
(define space "{\\ }")
(define big-space "{\\qquad}")
(define ellipsis "\\ldots")
(define err g-epsilon)
(define evaluates-to
  (string-append space "\\longrightarrow" space))
(define lpar "\\left(")
(define rpar "\\right)")
(define (env . args)
  (string-append
   g-Gamma
   (format "~a" (if (null? args)
                    ""
                    (string-append
                     "\\left["
                     (string-replace
                      (string-replace (string-join args "") "->" "\\mapsto")
                      "," (string-append "," space))
                     "\\right]")))
   space "\\vdash" space))
(define (group . args)
  (string-join
   #:before-first "{"
   args
   #:after-last "}"))
(define empty (group))
(define (parens . args)
  (string-join
   #:before-first lpar
   args
   #:after-last rpar))
(define (func . args)
  (match (string-split (string-join args "") ".")
    [(list var body)
     (parens g-lambda space (parens var) space body)]))
(define (sup . args)
  (match (string-split (string-join args "") "|")
    [(list var body)
     (parens g-sigma space var space body)]))

(define ($$judgment
         #:premise [premise #f]
         #:premises [premises (list)]
         #:conclusion [conclusion #f] . rest-conclusion)
  (let ([premises (add-between (map group (if premise
                                              (cons premise premises)
                                              premises))
                               big-space)]
        [conclusion (if conclusion
                        (cons conclusion rest-conclusion)
                        rest-conclusion)])
    ($$ (format "~a \\over ~a"
                (apply group premises)
                (apply group conclusion)))))
