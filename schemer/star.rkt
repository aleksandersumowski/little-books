#lang racket


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat)) (cond
                           ((eq? a (car lat))         (rember* a (cdr lat)))
                           (else                     (cons (car lat) (rember* a (cdr lat))))))             
      (else        (cons  (rember* a (car lat)) (rember* a (cdr lat)))))))

(define insertR*
  (lambda (old new lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat)) (cond
                           ((eq? old (car lat))         (cons old (cons new  (insertR* old new (cdr lat)))))
                           (else                        (cons (car lat)      (insertR* old new (cdr lat))))))             
      (else        (cons  (insertR* old new (car lat))  (insertR* old new (cdr lat)))))))

(define insertL*
  (lambda (old new l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                           ((eq? old (car l))         (cons old     (cons new  (insertL* old new (cdr l)))))
                           (else                      (cons (car l) (insertL* old new (cdr l))))))             
      (else        (cons  (insertL* old new (car l))  (insertL* old new (cdr l)))))))

(define add1
  (lambda (x)
    (+ 1 x)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                           ((eq? a (car l))         (add1 (occur* a (cdr l))))
                           (else                    (occur* a (cdr l)))))          
      (else        (+  (occur* a (car l))  (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond (eq? old (car l)) (cons new (subst* new old (cdr l)))
                             (else             (cons (car l) (subst* new old (cdr l))))))
      (else            (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))  (or (eq? a (car l))
                               (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))
                       

#;(member* 'chips        '((potato) (chips ((with fish) (chips)))))
#;(member* 'french-fries '((potato) (chips ((with fish) (chips)))))

(define leftmost
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? l) l)
          (else (leftmost (car l))))))

#;(leftmost '((potato) (chips ((with) fish) (chips))))

                


  

#;(rember* 'cup '((coffee) cup ((tea) cup)
           (and (hick)) cup))

#;(insertR* 'chuck 'roast '((how much (wood))
                         could
                         ((a (wood) chuck))
                         (((chuck)))
                         (if (a) ((wood chuck)))
                         could chuck wood))

#;(occur* 'banana '((banana)
         (split ((((banana ice))
                  (cream (banana))
                  sherbet))
                (banana)
                (bread)
                (banana brandy))))

