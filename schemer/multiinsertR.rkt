#lang racket

(require racket/block)

(define multinsertR
  (lambda (new old lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (block
                            (write lat)
                               (cons old (cons new (multinsertR new old (cdr lat))))))
      (else                (cons (car lat)
                                 (multinsertR new old (cdr lat)))))))

(multinsertR 'smallish 'cup '(coffee cup tea cup and hick cup))


(define multinsertL
  (lambda (new old lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (cons new (cons old (multinsertL new old (cdr lat)))))
      (else                (cons (car lat)
                                 (multinsertL new old (cdr lat)))))))

(multinsertL 'smallish 'cup '(coffee cup tea cup and hick cup))


(define multisubst
  (lambda (old new lat)
    (cond
      ((null? lat)         (quote ()))
      ((eq? old (car lat)) (cons new (multisubst old new (cdr lat))))
      (else                (cons (car lat)
                                 (multisubst old new (cdr lat)))))))

(multisubst 'cup 'glass '(coffee cup tea cup and hick cup))





