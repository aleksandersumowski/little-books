#lang racket
(include "equal.rkt")

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (or (eq? (car (cdr aexp)) '+)
                     (eq? (car (cdr aexp)) '*)
                     (eq? (car (cdr aexp)) '/))
                 (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) '+) (+ (value (car aexp))
                                    (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*) (* (value (car aexp))
                                    (value (car (cdr (cdr aexp)))))))))

(define 1st-sub-expr
  (lambda (x)
    (car (cdr x))))

(define 2nd-sub-expr
  (lambda (x)
    (car (cdr (cdr x)))))

(define operator
  (lambda (x)
    (car x)))

(define value2
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) '+) (+ (value2 (1st-sub-expr aexp))
                              (value2 (2nd-sub-expr aexp))))
      ((eq? (operator aexp) '*) (* (value2 (1st-sub-expr aexp))
                              (value2 (2nd-sub-expr aexp)))))))

(define 1st-sub-expr-2
    (lambda (x)
    (car x)))

(define 2nd-sub-expr-2
  (lambda (x)
    (car (cdr (cdr x)))))

(define operator-2
  (lambda (x)
    (car (cdr x))))

(define value3
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator-2 aexp) '+) (+ (value3 (1st-sub-expr-2 aexp))
                                     (value3 (2nd-sub-expr-2 aexp))))
      ((eq? (operator-2 aexp) '*) (* (value3 (1st-sub-expr-2 aexp))
                                     (value3 (2nd-sub-expr-2 aexp)))))))

(provide operator-2 1st-sub-expr-2 2nd-sub-expr-2 atom?)
#;(numbered? '(2 + (4 4 3)))
#;(numbered? '(2 + (4 / 3)))

#;(value3 '(4 + (5 * 3)))

#;(value3 '(5 * (3 + 2)))

#;(value2 '(+ 4 (* 5 3)))

#;(value2 '(* 5 (+ 3 2)))

