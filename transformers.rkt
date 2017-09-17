#lang racket

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
  

(define-syntax (make-connection-defns stx)
  (syntax-parse stx
    [(_ def ...)
     #`(begin
         #,@(for/list ([name (in-syntax #'(def ...))])
              (define dir
                (string->symbol
                 (second
                  (regexp-match
                   #px"connect-(.)"
                   (symbol->string (syntax->datum name))))))
              (with-syntax ([dir #`(quote #,dir)])
                #`(define-syntax (#,name stx)
                    (syntax-parse stx
                      [(_ p1 p2)
                       #`(begin
                           (send p1 add-connection p2 dir)
                           (send p2 add-connection p1 dir))])))))]))

(make-connection-defns connect-h connect-v connect-f connect-b)

(define-syntax (define-peg stx)
  (syntax-parse stx
    [(_ peg-id:id filled)
     #`(begin
         (define peg-id
           (make-peg
            #,(syntax-line #'peg-id)
            #,(syntax-column #'peg-id)
            filled))
         (send (current-game) add-peg peg-id))]))