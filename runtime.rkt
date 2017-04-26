#lang racket


(define peg%
  (class object%
    (super-new)
    (init-field
     position
     [filled? #t] ; default to #t since most slots on an initial board will be filled
     )
    (define selected? #f)
    ))