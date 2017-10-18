#lang racket

(require racket/syntax)
(require syntax/readerr)

(provide (rename-out [parse-board read-syntax]))

(module+ test
  (require rackunit)

  (define triangle
    (open-input-string
#<<>>
        O
       / \
      @   @
     / \ / \
    @ - @ - @
   / \ / \ / \
  @ - @ - @ - @
 /   / \ / \   \
@ - @ - @ - @ - @
>>
  )))

(define (connector? c)
  (define CONNECTORS '(#\- #\/ #\\ #\| #\X #\+))
  (and (member c CONNECTORS) #t))

(define (build-identifier sym source line column position span)
  (datum->syntax #f sym (list source line column position span)))

(define (get-connections connector)
  (case connector
    [(#\|) '(vertical-connection)]
    [(#\-) '(horizontal-connection)]
    [(#\+) '(vertical-connection horizontal-connection)]
    [(#\/) '(forward-connection)]
    [(#\\) '(backward-connection)]
    [(#\X) '(forward-connection backward-connection)]))

(define (get-offsets connector)
  (case connector
    [(vertical-connection) (values 0 -1 0 1)]
    [(horizontal-connection) (values -2 0 2 0)]
    [(forward-connection) (values 1 -1 -1 1)]
    [(backward-connection) (values 1 1 -1 -1)]))

(define (to-identifier sym name line col pos)
  (define stx-port (open-input-string (format "~s" sym)))
  (port-count-lines! stx-port)
  (set-port-next-location! stx-port line col pos)
  (fixup-span (read-syntax name stx-port)))

(define (fixup-span stx)
  (cond
    ;; work around what appears to be a bug in read-syntax or maybe ports
    [(and (exact-nonnegative-integer? (syntax-column stx))
          (exact-nonnegative-integer? (syntax-line stx)))
     (datum->syntax
      stx
      (syntax->datum stx)
      (list (syntax-source stx) (syntax-line stx)
            (syntax-column stx) (syntax-position stx)
            1)
      stx)]
    [else stx]))

(define (make-connection-syntax c name line col pos)
  (define connection (get-connections c))
  (for/list ([connection (in-list (get-connections c))])
    (define id (to-identifier connection name line col pos))
    (define-values (dx1 dy1 dx2 dy2) (get-offsets connection))
    `(,id ,(+ col dx1) ,(+ line dy1) ,(+ col dx2) ,(+ line dy2))))

(define (parse-board source in #:peg-char [peg-char #\@] #:hole-char [hole-char #\O])
  ;; TODO: should ensure peg-char and hole-char are not the same
  (define p-name (object-name in))
  (define name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous))
  (port-count-lines! in)
  (define body
    (let loop ([peg-stx null]
               [connection-stx null]
               [peg-count 0]
               [hole-count 0])
      (define-values (line col pos) (port-next-location in))
      (define c (read-char in))
      (cond
        [(eof-object? c) (reverse (append connection-stx peg-stx))]
        [(char-whitespace? c) (loop peg-stx connection-stx peg-count hole-count)]
        [(connector? c)
         (define new-connection-stx (make-connection-syntax c name line col pos))
         (loop peg-stx (append new-connection-stx connection-stx) peg-count hole-count)]
        [(eq? c peg-char)
         (define id (to-identifier (format-symbol "~a~a" c peg-count) name line col pos))
         (define new-peg-stx
           `(define-peg ,id ,col ,line #t))
         (loop (cons new-peg-stx peg-stx) connection-stx (add1 peg-count) hole-count)]
        [(eq? c hole-char)
         (define id (to-identifier (format-symbol "~a~a" c hole-count) name line col pos))
         (define new-hole-stx
           `(define-peg ,id ,col ,line #f))
         (loop (cons new-hole-stx peg-stx) connection-stx peg-count (add1 hole-count))]
        [else
         (raise-read-error (format "Unexpected pegs character: ~a" c) source line col pos 1)])))
  (datum->syntax #f `(,#'module ,name pegs ,@body)))
