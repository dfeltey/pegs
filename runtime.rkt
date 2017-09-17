#lang racket

(provide current-game
         initialize-game
         run-game
         make-peg)

(require racket/gui)

(define COORDINATE-SIZE 30)
(define OFFSET (/ COORDINATE-SIZE 2))

(define current-game (make-parameter #f))

(define (initialize-game) (new game%))
(define (run-game game) (send game run-game))

(define game%
  (class object%
    (super-new)

    (define the-pegs '())
    (define position-map #f)
    
    (define/public (add-peg p)
      (set! the-pegs (cons p the-pegs)))

    (define/public (run-game)
      (when (not (empty? the-pegs))
        (define-values (min-row max-row min-col max-col)
          (for/fold ([min-row #f]
                     [max-row #f]
                     [min-col #f]
                     [max-col #f])
                    ([peg (in-list the-pegs)])
            (define row (get-field row peg))
            (define col (get-field col peg)) 
            (values (if min-row (min min-row row) row)
                    (if max-row (max max-row row) row)
                    (if min-col (min min-col col) col)
                    (if max-col (max max-col col) col))))
        
        (define num-rows (+ 3 (- max-row min-row)))
        (define num-cols (+ 3 (- max-col min-col)))
        (define the-width (* num-cols COORDINATE-SIZE))
        (define the-height (* num-rows COORDINATE-SIZE))
        
        (define the-frame
          (new frame%
               [label "pegs"]
               [width the-width]
               [height the-height]
               [min-width the-width]
               [min-height the-height]))
        (define the-canvas
          (new canvas%
               [parent the-frame]
               [paint-callback
                (λ (canvas dc)
                  (for ([peg (in-list the-pegs)])
                    (send peg draw-connections dc min-row min-col))
                  (for ([peg (in-list the-pegs)])
                    (send peg draw dc min-row min-col)))]))
        (send the-frame show #t)))))

(define (make-peg row col filled?)
  (new peg% [row row] [col col] [filled? filled?]))

(define peg%
  (class object%
    (super-new)
    (init-field
     row
     col
     filled?) ; default to #t since most slots on an initial board will be filled
     (define selected? #f)
    ;; a hash table that stores the connections to other pegs
    ;; keyed by symbols
    (define connections (make-hash))
    (define/public (add-connection p dir)
      (hash-update!
       connections
       dir
       (λ (current) (cons p current))
       '()))

    (define/public (draw dc row-offset col-offset)
      (send dc set-pen "black" 3 'solid)
      (define color
        (cond
          [selected? "yellow"]
          [filled? "red"]
          [else "white"]))
      (send dc set-brush color 'solid)
      (send
       dc
       draw-ellipse
       (* COORDINATE-SIZE (add1 (- col col-offset)))
       (* COORDINATE-SIZE (add1 (- row row-offset)))
       COORDINATE-SIZE
       COORDINATE-SIZE))

    (define/public (draw-connections dc row-offset col-offset)
      (define this-x (+ OFFSET (* COORDINATE-SIZE (add1 (- col col-offset)))))
      (define this-y (+ OFFSET (* COORDINATE-SIZE (add1 (- row row-offset)))))
      (send dc set-pen "grey" 2 'solid)
      (for* ([(d ps) (in-hash connections)]
             [p (in-list ps)])
        (define that-x (+ OFFSET (* COORDINATE-SIZE (add1 (- (get-field col p) col-offset)))))
        (define that-y (+ OFFSET (* COORDINATE-SIZE (add1 (- (get-field row p) row-offset)))))
        (send dc draw-line this-x this-y that-x that-y)))))
