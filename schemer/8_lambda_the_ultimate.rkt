#lang racket
(require racket/block)
(require "equal.rkt")
(require "6_shadows.rkt")

(define rember-f
  (lambda (pred)
    (lambda (a lat)
      (cond
        ((null? lat)        (quote ()))
        ((pred a (car lat)) (cdr lat))
        (else               (cons (car lat) ((rember-f pred) a (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat)           (quote ()))
        ((test? old (car lat)) (cons old (cons new (cdr lat))))
        (else                  (cons (car lat)
                                     ((insertR-f test?) new old (cdr lat))))))))
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat)           (quote ()))
        ((test? old (car lat)) (cons new lat))
        (else                  (cons (car lat)
                                     ((insertL-f test?) new old (cdr lat))))))))

(define insert-unified
  (lambda (test? inserter)
    (lambda (new old lat)
      (cond
        ((null? lat)           (quote ()))
        ((test? old (car lat)) (inserter new old lat))
        (else                  (cons (car lat)
                                     ((insert-unified test? inserter) new old (cdr lat))))))))


(define insert-L-constructed
  (insert-unified eq? (lambda (new old lat) (cons new lat))))

(define insert-R-constructed
  (insert-unified eq? (lambda (new old lat) (cons old (cons new (cdr lat))))))

(define rember-constructed
  (insert-unified eq? (lambda (new old lat) (cdr lat))))

(define atom-to-fn
  (lambda (a)
    (cond
      ((eq? a '+) +)
      ((eq? a '*) *)
      ((eq? a '/) /))))

(define value4
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      (else ((atom-to-fn (operator-2 aexp))
             (value4 (1st-sub-expr-2 aexp))
             (value4 (2nd-sub-expr-2 aexp)))))))

(define multimember
  (lambda (pred lat)
    (cond
      ((null? lat)      (quote ()))
      ((pred (car lat)) (multimember pred (cdr lat)))
      (else             (cons (car lat) (multimember pred (cdr lat)))))))

(define multimember&co
  (lambda (a lat col)
    (cond
      ((null? lat)       (col (quote ()) (quote ())))
      ((eq? (car lat) a) (multimember&co a (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat (cons (car lat) seen)))))
      (else              (multimember&co a (cdr lat)
                                         (lambda (newlat seen)
                                           (col (cons (car lat) newlat) seen)))))))

(define a-friend?
  (lambda (x y)
    (null? y)))

(define multiinsert&co
  (lambda (new oldL oldR lat col)
      (cond
        ((null? lat)          (col (quote ()) 0 0))
        ((eq? (car lat) oldL) (multiinsert&co new oldL oldR (cdr lat) (lambda (newlat l-count r-count)
                                                                           (col (cons new (cons oldL newlat)) (add1 l-count) r-count))))
        ((eq? (car lat) oldR) (multiinsert&co new oldL oldR (cdr lat) (lambda (newlat l-count r-count)
                                                                           (col (cons oldR (cons new newlat)) l-count (add1 r-count)))))
        (else                 (multiinsert&co new oldL oldR (cdr lat) (lambda (newlat l-count r-count)
                                                                           (cons ( (cons (car lat) newlat) l-count r-count))))))))

(define add1
  (lambda (x)
    (+ x 1)))

(define ./
  (lambda (x y)
    (cond
      ((< x y) 0)
      (else    (add1 (./ (- x y) y))))))

(define even?
  (lambda (x)
    (= (* (./ x 2)
           2)
       x)))

(define evens-only*
  (lambda (lln)
    (cond
      ((null? lln)            (quote ()))
      ((and (atom? (car lln))
            (even? (car lln))) (cons (car lln)
                                     (evens-only* (cdr lln))))
      ((atom? (car lln))       (evens-only* (cdr lln)))
      (else                    (cons (evens-only* (car lln))
                                     (evens-only* (cdr lln)))))))


(define the-last-friend
  (lambda (col mul sum)
    (cons sum
          (cons mul
                col))))
  
(define evens-only*&co
  (lambda (lln col)
    (cond
      ((null? lln)             (col (quote ()) 1 0))
      ((and (atom? (car lln))
            (even? (car lln))) (evens-only*&co (cdr lln) (lambda (newlln mul-even sum-odd)
                                                           (col (cons (car lln) newlln)
                                                                (* mul-even
                                                                   (car lln))
                                                                sum-odd))))
      ((atom? (car lln))       (evens-only*&co (cdr lln) (lambda  (newlln mul-even sum-odd)
                                                           (col newlln
                                                                mul-even
                                                                (+ sum-odd
                                                                   (car lln))))))
      (else                    (evens-only*&co (car lln)  (lambda (car-newlln car-mul-even car-sum-odd)
                                                           (evens-only*&co (cdr lln)
                                                                           (lambda (cdr-newlln cdr-mul-even cdr-sum-odd)
                                                                             (col (cons car-newlln cdr-newlln)
                                                                                  (* car-mul-even
                                                                                     cdr-mul-even)
                                                                                  (+ car-sum-odd
                                                                                     cdr-sum-odd) )))))))))

(evens-only*&co '((2 3 4) ((3 5 6) 7 8)) the-last-friend)
(evens-only*&co '((9 1 2 8)
                  3 10 ((9 9) 7 6) 2) the-last-friend)
#;(evens-only* '((2 3 4) ((3 5 6) 7 8)))

#;(multimember&co 'test '(test test test) a-friend?)
#;(multimember (lambda (x) (= x 5)) '(4 5 6 5 666 7 5))

#;(value4 '(4 + (5 * 3)))

#;(value4 '(5 * (3 + 2)))


#;(insert-L-constructed 'new 'old '(t das old dsa))
#;(insert-R-constructed 'new 'old '(t das old dsa))
#;(rember-constructed   #f   'old '(t das old dsa))

#;((rember-f =) 3 '(6 2 5 5 3))
#;((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
