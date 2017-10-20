#lang racket/gui

(provide
 (contract-out
  [current-game (parameter/c game/c)]
  [initialize-game (-> game/c)]
  [run-game (->* () (game/c) void?)]
  [make-peg (-> integer? integer? boolean? peg/c)]
  [add-peg-to-game (->* (peg/c) (game/c) void?)]
  [forward-connection (-> peg/c peg/c void?)]
  [backward-connection (-> peg/c peg/c void?)]
  [horizontal-connection (-> peg/c peg/c void?)]
  [vertical-connection (-> peg/c peg/c void?)]))

(define game/c (instanceof/c (recursive-contract game%/c)))
(define peg/c (instanceof/c (recursive-contract peg%/c)))

(define connection/c
  (or/c 'forward-connection
        'backward-connection
        'horizontal-connection
        'vertical-connection))

(define game%/c
  (class/c
   [add-peg (->m peg/c void?)]
   [run-game (->m void?)]
   [handle-button-down (->m integer? integer? void?)]))

(define peg%/c
  (class/c
   (init-field
    [row integer?]
    [col integer?]
    [filled? boolean?])
   [add-connection (->m peg/c connection/c void?)]
   [get-connections (->m connection/c (listof peg/c))]
   [unselect (->m void?)]
   [flip! (->m void?)]
   [is-filled? (->m boolean?)]
   [handle-click (->m integer? integer? (or/c peg/c #f) (or/c peg/c #f))]
   [draw (->m (is-a?/c dc<%>) integer? integer? void?)]
   [draw-connections (->m (is-a?/c dc<%>) integer? integer? void?)]))

(define COORDINATE-SIZE 30)
(define OFFSET (exact-floor (/ COORDINATE-SIZE 2)))

(define current-game (make-parameter #f))
(define (initialize-game) (new game%))
(define (run-game [game (current-game)])
  (if game
      (send game run-game)
      (error 'run-game "No Game Found")))
(define (make-peg col row filled?)
  (new peg% [row row] [col col] [filled? filled?]))
(define (add-peg-to-game peg [game (current-game)])
  (if game
      (send game add-peg peg)
      (error 'add-peg-to-game "No Game Found")))
(define (connect-pegs p1 p2 dir)
  (begin
    (send p1 add-connection p2 dir)
    (send p2 add-connection p1 dir)))
(define-syntax-rule (define-connection-functions connect-name ...)
  (begin
    (define (connect-name p1 p2)
      (connect-pegs p1 p2 'connect-name))
    ...))
(define-connection-functions
  forward-connection
  backward-connection
  horizontal-connection
  vertical-connection)

(define game-canvas%
  (class canvas%
    (init-field game)
    (super-new)
    (define/override (on-subwindow-event window evt)
      (cond
        [(send evt button-down? 'left)
         (send game handle-button-down (send evt get-x) (send evt get-y))
         (send this refresh)
         #t]
        [else #f]))))

(define game%
  (class object%
    (super-new)
    (define the-pegs '())
    (define position-map #f)
    (define selected #f)
    (define game-canvas #f)
    (define y-offset 0)
    (define x-offset 0)
    
    (define/public (add-peg p)
      (set! the-pegs (cons p the-pegs)))

    (define/public (handle-button-down x y)
      (define col (+ x-offset (floor (sub1 (/ x COORDINATE-SIZE)))))
      (define row (+ y-offset (floor (sub1 (/ y COORDINATE-SIZE)))))
      (set! selected
            (or
             (for/or ([p (in-list the-pegs)])
               (send p handle-click row col selected))
             selected)))

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
        (set! y-offset min-row)
        (set! x-offset min-col)
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
          (new game-canvas%
               [parent the-frame]
               [game this]
               [paint-callback
                (λ (canvas dc)
                  (for ([peg (in-list the-pegs)])
                    (send peg draw-connections dc min-row min-col))
                  (for ([peg (in-list the-pegs)])
                    (send peg draw dc min-row min-col)))]))
        (send the-frame show #t)))))

(define peg%
  (class object%
    (super-new)
    (init-field row col filled?)
    (define selected? #f)
    ;; a hash table that stores the connections to other pegs
    ;; keyed by symbols
    (define connections (make-hash))
    (define/public (add-connection p dir)
      (hash-update! connections dir (λ (current) (set-add current p)) null))

    (define/public (unselect)
      (set! selected? #f))

    (define/public (flip!)
      (set! filled? (not filled?)))
    (define/public (is-filled?)
      filled?)
    (define/public (get-connections dir)
      (hash-ref connections dir null))

    (define/private (get-dir x1 y1 x2 y2)
      (cond
        [(= x1 x2) 'horizontal-connection]
        [(= y1 y2) 'vertical-connection]
        [(or (and (> x1 x2) (< y1 y2))
             (and (< x1 x2) (> y1 y2)))
         'forward-connection]
        [(or (and (> x1 x2) (> y1 y2))
             (and (< x1 x2) (< y1 y2)))
         'backward-connection]
        [else #f]))

    (define/public (handle-click x y selected-peg)
      (and (= x row) (= y col)
           (cond
             [filled?
              (and selected-peg (send selected-peg unselect))
              (set! selected? #t)
              this]
             [selected-peg
              (define that-x (get-field row selected-peg))
              (define that-y (get-field col selected-peg))
              (define dir (get-dir x y that-x that-y))
              (and dir
                   (let* ([this-connections (get-connections dir)]
                          [that-connections (send selected-peg get-connections dir)]
                          [common (set-intersect this-connections that-connections)]
                          [middle (and (not (set-empty? common)) (set-first common))]
                          [should-remove? (and middle (send middle is-filled?))])
                     (cond
                       [should-remove?
                        (flip!) ;; fill this one ...
                        (set! selected? #t) ;; select this one ...
                        (send middle flip!) ;; remove the middle one
                        (send selected-peg unselect)
                        (send selected-peg flip!) ;; remove the one from before
                        this]
                       [else #f])))]
             [else #f])))

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
      (send dc set-pen "gray" 2 'solid)
      (for* ([(d ps) (in-hash connections)]
             [p (in-set ps)])
        (define that-x (+ OFFSET (* COORDINATE-SIZE (add1 (- (get-field col p) col-offset)))))
        (define that-y (+ OFFSET (* COORDINATE-SIZE (add1 (- (get-field row p) row-offset)))))
        (send dc draw-line this-x this-y that-x that-y)))))
