(define-module (minikanren examples lists)
  #:export (membero not-membero
		    appendo selecto
		    mapo assoco
		    
		    same-lengtho
		    make-assoc-tableo
			MAPO-WITH-ACCUMULATOR?
			MAPO-WITH-RECURSION-FIRST?
		    ))
(use-modules (minikanren language))

(define MAPO-WITH-ACCUMULATOR? #t)
(define MAPO-WITH-RECURSION-FIRST? #f)

(define (membero x l)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car))
	   ((membero x cdr)))))

(define (not-membero x l)
  (conde ((== l '()))
	 ((fresh (car cdr)
	    (== l `(,car . ,cdr))
	    (=/= x car)
	    (not-membero x cdr)))))

(define (appendo xs ys zs)
  (conde ((== xs '()) (== ys zs))
	 ((fresh (x-head x-tail z-tail)
	    (== xs `(,x-head . ,x-tail))
	    (== zs `(,x-head . ,z-tail))
	    (appendo x-tail ys z-tail)))))

(define (selecto x l l-x)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (conde ((== x car)
	    (== l-x cdr))
	   ((fresh (cdr-x)
	      (== l-x `(,car . ,cdr-x))
	      (selecto x cdr cdr-x))))))

(define mapo
  (if MAPO-WITH-ACCUMULATOR?
	  (letrec
		  ((mapo
			(lambda* (p l #:optional (acc (== 'cat 'cat)))
			  (conde
			   [(== l '())
				acc]
			   [(fresh (car cdr)
				  (== l `(,car . ,cdr))
				  (mapo p cdr (fresh () acc (p car))))]))))
		mapo)
	  (if MAPO-WITH-RECURSION-FIRST?
		  (letrec
			((mapo
			  (lambda (p l)
				(conde ((== l '()))
				  ((fresh (car cdr)
					 (== l `(,car . ,cdr))
					 (mapo p cdr)
					 (p car)))))))
			mapo)
		  (letrec
			((mapo
			  (lambda (p l)
				(conde ((== l '()))
				  ((fresh (car cdr)
					 (== l `(,car . ,cdr))
					 (p car)
					 (mapo p cdr)))))))
			mapo))))

(define (assoco key table value)
  (fresh (car table-cdr)
    (== table `(,car . ,table-cdr))
    (conde ((== `(,key . ,value) car))
	   ((assoco key table-cdr value)))))

(define (same-lengtho l1 l2)
  (conde ((== l1 '()) (== l1 '()))
	 ((fresh (car1 cdr1 car2 cdr2)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (same-lengtho cdr1 cdr2)))))

(define (make-assoc-tableo l1 l2 table)
  (conde ((== l1 '()) (== l1 '()) (== table '()))
	 ((fresh (car1 cdr1 car2 cdr2 cdr3)
	    (== l1 `(,car1 . ,cdr1))
	    (== l2 `(,car2 . ,cdr2))
	    (== table `((,car1 . ,car2) . ,cdr3))
	    (make-assoc-tableo cdr1 cdr2 cdr3)))))
