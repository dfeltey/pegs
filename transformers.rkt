#lang racket

(provide (rename-out [pegs-module-begin #%module-begin])
         (except-out (all-defined-out) pegs-module-begin))

(require (for-syntax syntax/parse racket))
(require "runtime.rkt" racket/stxparam syntax/parse/define)

(define-syntax-parameter position-table
  (λ (stx)
    (raise-syntax-error 'position-table "used outside pegs module" stx)))

(define-syntax (pegs-module-begin stx)
  (syntax-parse stx
    [(_  body ...)
     #'(#%module-begin
        (define the-game (initialize-game))
        (syntax-parameterize ([position-table (make-hash)])
          (parameterize ([current-game the-game])
            body ...
            (run-game the-game))))]))

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ peg-id:id x:integer y:integer filled)
     (define table (syntax-parameter-value #'position-table))
     (hash-set! table (cons (syntax-e #'x) (syntax-e #'y)) #'peg-id)
     #`(begin
         (define peg-id (make-peg x y filled))
         (add-peg-to-game peg-id))]))

(define-syntax (make-connection stx)
  (syntax-parse stx
    [(_ x1:integer y1:integer x2:integer y2:integer dir)
     (define table (syntax-parameter-value #'position-table))
     (define pos1 (cons (syntax-e #'x1) (syntax-e #'y1)))
     (define pos2 (cons (syntax-e #'x2) (syntax-e #'y2)))
     (define peg1
       (hash-ref
        table
        pos1
        (lambda () (raise-syntax-error 'make-connection "No peg found in connection"))))
     (define peg2
       (hash-ref
        table
        pos2
        (lambda () (raise-syntax-error 'make-connection "No peg found in connection"))))
     #`(connect-pegs #,peg1 #,peg2 dir)]))

(define-simple-macro (define-connection-macro name key)
  (define-simple-macro (name x1:integer y1:integer x2:integer y2:integer)
    (make-connection x1 y1 x2 y2 'key)))

(define-connection-macro forward-connection forward)
(define-connection-macro backward-connection backward)
(define-connection-macro horizontal-connection horizontal)
(define-connection-macro vertical-connection vertical)
