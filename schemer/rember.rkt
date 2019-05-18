#lang racket

(define rember
  (lambda (a lat)
    (cond
      ((null? lat)        (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))


(rember 'and '(bacon lettuce and tomato))

