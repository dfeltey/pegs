#lang racket

(provide (rename-out [pegs-module-begin #%module-begin])
         (except-out (all-defined-out) pegs-module-begin))

(require "runtime.rkt")

(define-syntax (pegs-module-begin stx)
  #'(#%module-begin (void)))
  

(define-syntax (make-default-defns stx)
  (syntax-case stx ()
    [(_ t ...) #'(begin (define-syntax (t stx) #'(void)) ...)]))

(make-default-defns define-peg connect-h connect-v connect-f connect-b)