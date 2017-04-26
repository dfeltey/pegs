#lang racket

(provide (all-defined-out))

(require "runtime.rkt")

(define-syntax (pegs-module-begin stx)
  #'(#%module-begin #'(void)))
  

(define-syntax (make-default-defns stx)
  (syntax-case stx ()
    [(_ t ...) #'(begin (define-syntax (t stx) #'(void)) ...)]))

(make-default-defns connect-h connect-v connect-f connect-b)