#lang racket

(require racket/syntax)

(define triangle
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
  )

#|
 Processing should return a list of the nodes,
 a list of the connections in the form of
 - (vertical node1 node2)
 - (horizontal node1 node2)
 - (forward node1 node2) (forward as in forward slash)
 - (back node1 node2)

 `X`s should be allowed to stand for both foreward and back
  TODO: type checking or something to highlight useless edges 


|#

(struct peg (id filled?) #:transparent)
(struct game (pegs connections) #:transparent)
;; FIXME: these should represent connections between peg objects ...
(struct connection (line column position) #:transparent)
(struct horizontal connection () #:transparent)
(struct vertical connection () #:transparent)
(struct forward connection () #:transparent)
(struct back connection () #:transparent)

(define (connector? c)
  (define CONNECTORS '(#\- #\/ #\\ #\| #\X #\+))
  (and (member c CONNECTORS) #t))

(define (make-connections c line col pos)
  (case c
    [(#\-) (list (horizontal line col pos))]
    [(#\|) (list (vertical line col pos))]
    [(#\+) (list (horizontal line col pos)
                 (vertical line col pos))]
    [(#\/) (list (forward line col pos))]
    [(#\\) (list (back line col pos))]
    [(#\X) (list (forward line col pos)
                 (back line col pos))]))

(define (build-identifier sym source line column position span)
  (datum->syntax #f sym (list source line column position span)))
    

(define (parse-board source
                     board-str
                     #:peg-char [peg-char #\@]
                     #:hole-char [hole-char #\O])
    
  (define board-port (open-input-string board-str))
  (port-count-lines! board-port)
  (define-values (start-line start-column start-pos) (port-next-location board-port))
  (define-values (pegs peg-hash connections)
    (let loop ([pegs '()]
               [peg-hash (hash)]
               [connections '()]
               [tag 0])
      (define-values (line column pos) (port-next-location board-port))
      (define c (read-char board-port))
      (cond
        [(eof-object? c) (values (reverse pegs) peg-hash (reverse connections))]
        [(char-whitespace? c)
         (loop pegs peg-hash connections tag)]
        [(connector? c)
         (loop pegs peg-hash (append (make-connections c line column pos) connections) tag)]
        [(or (eq? c peg-char) (eq? c hole-char))
         (define id (build-identifier (format-symbol "~a~a" c tag) source line column pos 1))
         (define spot (peg id (eq? c peg-char)))
         (define peg-hash* (hash-set peg-hash (cons line column) id))
         (loop (cons spot pegs) peg-hash* connections (add1 tag))]
        [else (error 'parse-board "unexpected char in port: ~a" c)])))
  (define-values (end-line end-column end-pos) (port-next-location board-port))
  (define peg-stx
    (for/list ([p (in-list pegs)])
      (match-define (peg id filled?) p)
      (datum->syntax #f `(define-peg ,id ,filled?))))
  (define connections-stx
    (for/list ([c (in-list connections)])
      (match-define (connection line col pos) c)
      (datum->syntax
       #f
       (cond
         [(horizontal? c)
          ;; the 2s here are annoying, but horizontal connections don't look good otherwise
          ;; without more work
          (define fst (hash-ref peg-hash (cons line (- col 2))))
          (define snd (hash-ref peg-hash (cons line (+ col 2))))
          `(connect-h ,fst ,snd)]
         [(vertical? c)
          (define fst (hash-ref peg-hash (cons (sub1 line) col)))
          (define snd (hash-ref peg-hash (cons (add1 line) col)))
          `(connect-v ,fst ,snd)]
         [(forward? c)
          (define fst (hash-ref peg-hash (cons (sub1 line) (add1 col))))
          (define snd (hash-ref peg-hash (cons (add1 line) (sub1 col))))
          `(connect-f ,fst ,snd)]
         [(back? c)
          (define fst (hash-ref peg-hash (cons (sub1 line) (sub1 col))))
          (define snd (hash-ref peg-hash (cons (add1 line) (add1 col))))
          `(connect-b ,fst ,snd)]))))
  (datum->syntax #f `(,#'module ,#'source pegs ,@peg-stx ,@connections-stx)))
    
   
  
                     
