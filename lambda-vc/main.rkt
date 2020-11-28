#lang racket

(require "lambda-vc.rkt")

(provide (all-from-out "lambda-vc.rkt"))

(module reader syntax/module-reader
  varcur-calc)
