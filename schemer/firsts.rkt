#lang racket

(define firsts
  (lambda (lal)
    (cond
      ((null? lal)  (quote ()))
      (else        (cons (car (car lal)) (firsts (cdr lal)))))))

#;(firsts '((a b) (c d) (e f)))

(provide firsts)
