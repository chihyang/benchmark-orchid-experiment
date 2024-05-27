#lang racket

(provide membero not-membero
         appendo selecto
         mapo assoco
         same-lengtho
         make-assoc-tableo
         MAPO-WITH-ACCUMULATOR?
         MAPO-WITH-RECURSION-FIRST?
         )

(require "../mk.rkt")

(define MAPO-WITH-ACCUMULATOR? #t)
(define MAPO-WITH-RECURSION-FIRST? #f)

(defrel (membero x l)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (disj (== x car) (membero x cdr))))

(defrel (not-membero x l)
  (disj
    (== l '())
    (fresh (car cdr)
      (== l `(,car . ,cdr))
      (=/= x car)
      (not-membero x cdr))))

(defrel (appendo xs ys zs)
  (disj
    (conj (== xs '()) (== ys zs))
    (fresh (x-head x-tail z-tail)
      (== xs `(,x-head . ,x-tail))
      (== zs `(,x-head . ,z-tail))
      (appendo x-tail ys z-tail))))

(defrel (selecto x l l-x)
  (fresh (car cdr)
    (== l `(,car . ,cdr))
    (disj
      (conj (== x car) (== l-x cdr))
      (fresh (cdr-x)
        (== l-x `(,car . ,cdr-x))
        (selecto x cdr cdr-x)))))

(defrel (mapo-acc-in p l acc)
  (disj
    (conj (== l '()) acc)
    (fresh (car cdr)
      (== l `(,car . ,cdr))
      (mapo-acc-in p cdr (fresh () acc (p car))))))

(defrel (mapo-acc p l)
  (mapo-acc-in p l succeed))

(defrel (mapo-recursion-first p l)
  (disj
    (== l '())
    (fresh (car cdr)
      (== l `(,car . ,cdr))
      (mapo-recursion-first p cdr)
      (p car))))

(defrel (mapo-recursion-last p l)
  (disj
    (== l '())
    (fresh (car cdr)
      (== l `(,car . ,cdr))
      (p car)
      (mapo-recursion-last p cdr))))

(define mapo
  (cond
    (MAPO-WITH-ACCUMULATOR? mapo-acc)
    (MAPO-WITH-RECURSION-FIRST? mapo-recursion-first)
    (else mapo-recursion-last)))

(defrel (assoco key table value)
  (fresh (car table-cdr)
    (== table `(,car . ,table-cdr))
    (disj
      (== `(,key . ,value) car)
      (assoco key table-cdr value))))

(defrel (same-lengtho l1 l2)
  (disj
    (conj (== l1 '()) (== l1 '()))
    (fresh (car1 cdr1 car2 cdr2)
      (== l1 `(,car1 . ,cdr1))
      (== l2 `(,car2 . ,cdr2))
      (same-lengtho cdr1 cdr2))))

(defrel (make-assoc-tableo l1 l2 table)
  (disj
    (conj (== l1 '()) (== l1 '()) (== table '()))
    (fresh (car1 cdr1 car2 cdr2 cdr3)
      (== l1 `(,car1 . ,cdr1))
      (== l2 `(,car2 . ,cdr2))
      (== table `((,car1 . ,car2) . ,cdr3))
      (make-assoc-tableo cdr1 cdr2 cdr3))))
