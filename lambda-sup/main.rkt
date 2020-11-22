#lang racket

(require "base.rkt")

(provide (all-from-out "base.rkt"))

(module reader syntax/module-reader
  varcur-calc)
