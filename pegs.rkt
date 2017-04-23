#lang racket

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

(struct peg (char tag filled? line column position) #:transparent)
(struct game (pegs connections) #:transparent)
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
    

(define (parse-board board-str
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
        [(eof-object? c) (values pegs peg-hash connections)]
        [(char-whitespace? c)
         (loop pegs peg-hash connections tag)]
        [(connector? c)
         (loop pegs peg-hash (append (make-connections c line column pos) connections) tag)]
        [(or (eq? c peg-char) (eq? c hole-char))
         (define spot (peg c tag (eq? c peg-char) line column pos))
         (define peg-hash* (hash-set peg-hash (cons line column) spot))
         (loop (cons spot pegs) peg-hash* connections (add1 tag))]
        [else (error 'parse-board "unexpected char in port: ~a" c)])))
  (define-values (end-line end-column end-pos) (port-next-location board-port))

  pegs)
                     
