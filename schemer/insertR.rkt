#lang racket

(require racket/block)

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else                (cons (car lat)
                                 (insertR new old (cdr lat))))
      )))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(taco tamales and salsa))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else                (cons (car lat)
                                 (insertL new old (cdr lat)))))
      ))


(insertL 'jalapeno 'and '(taco tamales and salsa))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else                (cons (car lat)
                                 (subst new old (cdr lat)))))))


(subst 'jalapeno 'and '(taco tamales and salsa))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? o1 (car lat)) (cons new (cdr lat)))
      ((eq? o2 (car lat)) (cons new (cdr lat)))
      (else                (cons (car lat)
                                 (subst2 new o1 o2 (cdr lat)))))))


(subst2 'jalapeno 'and 'taco '(taco tamales and salsa))
(subst2 'jalapeno  'salsa 'and '(taco tamales and salsa))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? a (car lat))   (multirember a (cdr lat)))
      (else                (cons (car lat)
                                 (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup))



