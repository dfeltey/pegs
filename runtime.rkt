#lang typed/racket/no-check

(provide current-game
         initialize-game
         run-game
         make-peg
         add-peg-to-game
         forward-connection
         backward-connection
         horizontal-connection
         vertical-connection)

(require typed/racket/gui)

(define-type Game%
  (Class
   [add-peg (-> Peg Void)]
   [run-game (-> Void)]
   [handle-button-down (-> Integer Integer Void)]))

(define-type Peg%
  (Class
   (init-field
    [row Integer]
    [col Integer]
    [filled? Boolean])
   [add-connection (-> Peg Connection Void)]
   [get-connections (-> Connection (Setof Peg))]
   [is-filled? (-> Boolean)]
   [unselect (-> Void)]
   [flip! (-> Void)]
   [handle-click (-> Integer Integer (Option Peg) (Option Peg))]
   [draw (-> (Instance DC<%>) Integer Integer Void)]
   [draw-connections (-> (Instance DC<%>) Integer Integer Void)]))

(define-type Game (Instance Game%))
(define-type Peg (Instance Peg%))

(define-type Connection
  (U 'forward-connection
     'backward-connection
     'horizontal-connection
     'vertical-connection))

(define-type Game-Canvas%
  (Class #:implements/inits Canvas%
         (init-field [game Game])))

(: COORDINATE-SIZE Natural)
(define COORDINATE-SIZE 30)

(: OFFSET Natural)
(define OFFSET (exact-floor (/ COORDINATE-SIZE 2)))

(: current-game (Parameterof (Option Game)))
(define current-game (make-parameter #f))

(: initialize-game (-> Game))
(define (initialize-game) (new game%))

(: run-game (-> Game Void))
(define (run-game game) (send game run-game))

(: make-peg (-> Integer Integer Boolean Peg))
(define (make-peg col row filled?)
  (new peg% [row row] [col col] [filled? filled?]))

(: add-peg-to-game (-> Peg Void))
(define (add-peg-to-game peg)
  (send (assert (current-game)) add-peg peg))

(: connect-pegs (-> Peg Peg Connection Void))
(define (connect-pegs p1 p2 dir)
  (begin
    (send p1 add-connection p2 dir)
    (send p2 add-connection p1 dir)))

(define-syntax-rule (define-connection-functions connect-name ...)
  (begin
    (: connect-name (-> Peg Peg Void))
    ...
    (define (connect-name p1 p2)
      (connect-pegs p1 p2 'connect-name))
    ...))

(define-connection-functions
  forward-connection
  backward-connection
  horizontal-connection
  vertical-connection)

(: game-canvas% Game-Canvas%)
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

(: game% Game%)
(define game%
  (class object%
    (super-new)
    (: the-pegs (Listof Peg))
    (define the-pegs '())
    (define position-map #f)
    (: selected (Option Peg))
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
             (for/or : (Option Peg) ([p : Peg (in-list the-pegs)])
               (send p handle-click row col selected))
             selected)))

    (define/public (run-game)
      (when (not (empty? the-pegs))
        (define-values (min-row max-row min-col max-col)
          (for/fold : (Values (Option Integer)
                              (Option Integer)
                              (Option Integer)
                              (Option Integer))
            ([min-row : (Option Integer) #f]
             [max-row : (Option Integer) #f]
             [min-col : (Option Integer) #f]
             [max-col : (Option Integer) #f])
            ([peg : Peg (in-list the-pegs)])
            (define row (get-field row peg))
            (define col (get-field col peg)) 
            (values (if min-row (min min-row row) row)
                    (if max-row (max max-row row) row)
                    (if min-col (min min-col col) col)
                    (if max-col (max max-col col) col))))
        (when (and min-row max-row min-col max-col)
          
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
                 [min-width (assert the-width positive?)]
                 [min-height (assert the-height positive?)]))
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
          (send the-frame show #t))))))

(: peg% Peg%)
(define peg%
  (class object%
    (super-new)
    (init-field row col filled?)
    (: selected? Boolean)
    (define selected? #f)
    ;; a hash table that stores the connections to other pegs
    ;; keyed by symbols
    (: connections (HashTable Connection (Setof Peg)))
    (define connections (make-hash))
    (define/public (add-connection p dir)
      ((inst hash-update! Connection (Setof Peg))
       connections
       dir
       (λ ([current : (Setof Peg)]) (set-add current p))
       (inst set Peg)))

    (define/public (unselect)
      (set! selected? #f))

    (define/public (flip!)
      (set! filled? (not filled?)))
    (define/public (is-filled?)
      filled?)
    (define/public (get-connections dir)
      ((inst hash-ref Connection (Setof Peg) (Setof Peg))
       connections
       dir
       (inst set Peg)))

    (: get-dir (-> Integer Integer Integer Integer (Option Connection)))
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
