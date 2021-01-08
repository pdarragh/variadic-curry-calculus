#lang racket

(require scribble/html)

(provide (all-defined-out))

;; 'text
;; 'username
;; url
;; date
(define (embed-tweet elements)
  (let ([text (cdr (assoc 'text elements))]
        [user (cdr (assoc 'user elements))]
        [url (cdr (assoc 'url elements))]
        [date (cdr (assoc 'date elements))])
    (string-append
     (xml->string
      (blockquote
       class: "twitter-tweet"
       (p lang: "en"
          dir: "ltr"
          text)
       mdash
       user
       (a href: url
          date)))
     (xml->string
      (script src: "https://platform.twitter.com/widgets.js"
              charset: "utf-8")))))

;; @embed-tweet[(list (cons 'text "If you had a Lisp with auto-currying what would you expect as the application of ((- 1) 2)?")
;;                    (cons 'user "ӺØⓖµƨ (@fogus)")
;;                    (cons 'url "https://twitter.com/fogus/status/1329097890359349248?ref_src=twsrc%5Etfw")
;;                    (cons 'date "November 18, 2020"))]
