#lang racket

(require racket/syntax)
(require syntax/readerr)

(provide (rename-out [parse-board read-syntax]))

(module+ test
  (require rackunit)

  (define small-triangle
    (open-input-string
#<<>>
        O
       / \
      @   @
     / \ / \
    @ - @ - @
>>
))
(check-equal?
 (syntax->datum (parse-board 'test small-triangle))
 '(module anonymous pegs
    (define-peg O0 8 1 #f)
    (define-peg @0 6 3 #t)
    (define-peg @1 10 3 #t)
    (define-peg @2 4 5 #t)
    (define-peg @3 8 5 #t)
    (define-peg @4 12 5 #t)
    (forward-connection O0 @0)
    (backward-connection O0 @1)
    (forward-connection @0 @2)
    (backward-connection @0 @3)
    (forward-connection @1 @3)
    (backward-connection @1 @4)
    (horizontal-connection @2 @3)
    (horizontal-connection @3 @4)))

  (define X
    (open-input-string
#<<>>
    @ @
     X
    @ @
>>
  ))

  (check-equal?
   (syntax->datum (parse-board 'test X))
   '(module anonymous pegs
      (define-peg @0 4 1 #t)
      (define-peg @1 6 1 #t)
      (define-peg @2 4 3 #t)
      (define-peg @3 6 3 #t)
      (backward-connection @0 @3)
      (forward-connection @1 @2)))

  (define PLUS
    (open-input-string
#<<>>
  @
@ + @
  @
>>
  ))

    (check-equal?
   (syntax->datum (parse-board 'test PLUS))
   '(module anonymous pegs
      (define-peg @0 2 1 #t)
      (define-peg @1 0 2 #t)
      (define-peg @2 4 2 #t)
      (define-peg @3 2 3 #t)
      (horizontal-connection @1 @2)
      (vertical-connection @0 @3)))

  (define LINE
    (open-input-string
#<<>>
@
|
@
|
O
>>
  ))

  (check-equal?
   (syntax->datum (parse-board 'test LINE))
   '(module anonymous pegs
      (define-peg @0 0 1 #t)
      (define-peg @1 0 3 #t)
      (define-peg O0 0 5 #f)
      (vertical-connection @0 @1)
      (vertical-connection @1 O0)))

  (check-exn (regexp "Unexpected pegs character: *")
             (lambda ()
               (parse-board 'test (open-input-string "*"))))

  (check-exn (regexp "Second peg missing in horizontal-connection")
             (lambda ()
               (parse-board 'test (open-input-string "@ - "))))

  (check-exn (regexp "First peg missing in vertical-connection")
             (lambda ()
               (parse-board 'test (open-input-string "|"))))

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

(struct input (line col pos))
(struct peg  input (name filled?))
(struct connection input (direction))

(define (connector? c)
  (define CONNECTORS '(#\- #\/ #\\ #\| #\X #\+))
  (and (member c CONNECTORS) #t))

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
    [(backward-connection) (values -1 -1 1 1)]))

(define (to-identifier c name line col pos)
  (define stx-port (open-input-string (format "~s" c)))
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

(define (parse-board source in #:peg-char [peg-char #\@] #:hole-char [hole-char #\O])
  ;; TODO: should ensure peg-char and hole-char are not the same
  (define p-name (object-name in))
  (define name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous))
  (port-count-lines! in)
  (define chars
    (let loop ([pegs null]
               [connections null]
               [peg-count 0]
               [hole-count 0])
      (define-values (line col pos) (port-next-location in))
      (define c (read-char in))
      (cond
        [(eof-object? c) (reverse (append connections pegs))]
        [(char-whitespace? c) (loop pegs connections peg-count hole-count)]
        [(connector? c)
         (define new-connections
           (map (lambda (dir) (connection line col pos dir)) (get-connections c)))
         (loop pegs (append new-connections connections) peg-count hole-count)]
        [(eq? c peg-char)
         (define the-peg
           (peg (format-symbol "~a~a" c peg-count) line col pos #t))
         (loop (cons the-peg pegs) connections (add1 peg-count) hole-count)]
        [(eq? c hole-char)
         (define the-hole
           (peg (format-symbol "~a~a" c hole-count) line col pos #f))
         (loop (cons the-hole pegs) connections peg-count (add1 hole-count))]
        [else
         (raise-read-error (format "Unexpected pegs character: ~a" c) source line col pos 1)])))
  (define body (pegs/connections->syntax chars name source))
  (datum->syntax #f `(,#'module ,name pegs ,@body)))

(define (pegs/connections->syntax chars name source)
  (define id-table (make-hash))
  (for/list ([c (in-list chars)])
    (match c
      [(peg char line col pos filled?)
       (define id (to-identifier char name line col pos))
       (hash-set! id-table (cons col line) id)
       `(define-peg ,id ,col ,line ,filled?)]
      [(connection line col pos dir)
       (define id (to-identifier dir name line col pos))
       (define-values (dx1 dy1 dx2 dy2) (get-offsets dir))
       (define key1 (cons (+ col dx1) (+ line dy1)))
       (define key2 (cons (+ col dx2) (+ line dy2)))
       (define peg1-id
         (hash-ref
          id-table
          key1
          (lambda ()
            (raise-read-error (format "First peg missing in ~a" dir) source line col pos))))
       (define peg2-id
         (hash-ref
          id-table
          key2
          (lambda ()
            (raise-read-error (format "Second peg missing in ~a" dir) source line col pos 1))))
       `(,dir ,peg1-id ,peg2-id)])))
