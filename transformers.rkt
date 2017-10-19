#lang typed/racket/no-check

(provide (rename-out [pegs-module-begin #%module-begin])
         (except-out (all-defined-out) pegs-module-begin))

(require (for-syntax syntax/parse racket))
(require "runtime.rkt")

(define-syntax (pegs-module-begin stx)
  (syntax-parse stx
    [(_  body ...)
     #'(#%module-begin
        (define the-game (initialize-game))
        (parameterize ([current-game the-game])
          body ...
          (run-game the-game)))]))

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ peg-id:id x:integer y:integer filled)
     #`(begin
         (define peg-id (make-peg x y filled))
         (add-peg-to-game peg-id))]))
