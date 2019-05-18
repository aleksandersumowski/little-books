#lang racket

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
