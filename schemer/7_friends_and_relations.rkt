#lang racket

(require "firsts.rkt")

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (e lat)
    (cond ((null? lat) #f)
          ((eq? e (car lat)) #t)
          (else (member? e (cdr lat))))))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(define multimember
  (lambda (e lat)
    (cond
      ((null? lat)       (quote ()))
      ((eq? e (car lat)) (multimember e (cdr lat)))
      (else              (cons (car lat)
                               (multimember e (cdr lat)))))))

(define makeset
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((member? (car l) (cdr l)) (makeset (cdr l)))
      (else (cons (car l) (makeset (cdr l)))))))

(define makeset-2
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car l)
                  (makeset-2 (multimember (car l) (cdr l))))))))

(define subset?
  (lambda (l1 l2)
    (cond
      ((null? l1) #t)
      (else (and (member? (car l1) l2)
                 (subset? (cdr l1) l2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1)
                                       (intersect (cdr set1)
                                                  set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      (else (cons (car set1) (union (cdr set1) (multimember (car set1) set2)))))))

(define union-2
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else    (cons (car set1) (union (cdr set1) set2))))))

(define xxx
  (lambda (set1 set2)
    (cond ((null? set1) (quote ()))
          ((member? (car set1) set2) (xxx (cdr set1) set2))
          (else (cons (car set1) (xxx (cdr set2) set2))))))

(define intersectall
  (lambda (lal)
    (cond
      ((null? (cdr lal)) (car lal))
      (else              (intersect (car lal)
                                    (intersectall (cdr lal)))))))

(define a-pair?
  (lambda (s)
    (and (not (null? s))
         (not (atom? s))
         (not (null? (cdr s)))
         (null? (cdr (cdr s))))))

(define rel?
  (lambda (lap)
    (set? (firsts lap))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (a1 a2)
    (cons a1 (cons a2 '()))))

(define revpair
  (lambda (p)
    (build (second p)
           (first p))))

(define revrel
  (lambda (lap)
    (cond
      ((null? lap) (quote ()))
      (else        (cons (revpair (car lap))
                         (revrel (cdr lap)))))))

(define seconds
  (lambda (lap)
    (cond
      ((null? lap) (quote ()))
      (else (cons (second (car lap))
                  (cdr lap))))))

(define fullfun?
  (lambda (lap)
    (set? (seconds lap))))


#;(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))



#;(revrel '((8 a) (pie pumpkin) (sick got)))
#;(rel? '((b 1) (b 2)))
#;(rel? '((a 1) (b 2)))

#;(a-pair? '())
#;(a-pair? '(2))
#;(a-pair? '(2 3))
#;(a-pair? '(a b c d))

#;(intersectall '((6 peaches and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

#;(union-2 '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

#;(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

#;(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))

#;(set? '(2 3 4))
#;(set? '(a b a))

#;(multimember '3 '(3 4 dada 3))

#;(makeset-2 '(2 3 4))
#;(makeset-2 '(a b a cdsd))
#;(subset? '(4 pounds of horseradish)
         '(four pounds of chicken and 5 ounces of horseradish))
#;(subset? '(5 chicken wings)
         '(5 hamburgers
             2 pieces of chicken and
             light duckling wings))
