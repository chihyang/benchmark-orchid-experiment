#lang racket

(require "../mk.rkt")
(provide valofo quine twine thine)

;;; For a branch that cannot succeed, we can
;;; eliminate it from the relation.
(defrel (lookupo senv venv y o)
  (fresh (sa sd va vd)
    (conj
      (== `((,sa . ,sd) (,va . ,vd)) `(,senv ,venv))
      (disj
        (conj (== sa y) (== va o))
        (conj (=/= sa y)
              (lookupo sd vd y o))))))

(defrel (not-in-envo x vars)
  (disj
    (conj (== '() vars) succeed)
    (fresh (y ys)
      (conj
        (== `(,y . ,ys) vars)
        (=/= y x)
        (not-in-envo x ys)))))

(defrel (valofo expr vars vals o)
  (disj
    (fresh (v)
      (conj
        (== `(quote ,v) expr)
        ;; this can be replaced with absento
        (not-in-envo 'quote vars)
        (absento 'clos v)
        (== v o)))
    (fresh (a*)
      (conj
        (== `(list . ,a*) expr)
        (not-in-envo 'list vars)
        (absento 'clos a*)
        (valof-listo a* vars vals o)))
    (conj
      (symbolo expr)
      (lookupo vars vals expr o))
    (fresh (x body)
      (conj
        (== `(λ (,x) ,body) expr)
        (symbolo x)
        (== `(clos ,x ,body ,vars ,vals) o)
        (not-in-envo 'λ vars)))
    (fresh (rator rand rator-res rand-res x body vars^ vals^)
      (conj
        (== `(,rator ,rand) expr)
        (== `(clos ,x ,body ,vars^ ,vals^) rator-res)
        ;; move recursive calls to bottom
        (valofo rator vars vals rator-res)
        (valofo rand vars vals rand-res)
        (valofo body
                `(,x . ,vars^)
                `(,rand-res . ,vals^)
                o)))))

(defrel (valof-listo exprs vars vals o)
  (disj
    (conj (== '() exprs)
          (== '() o))
    (fresh (a d car-res cdr-res)
      (conj
        (== `(,a . ,d) exprs)
        (== `(,car-res . ,cdr-res) o)
        (valofo a vars vals car-res)
        (valof-listo d vars vals cdr-res)))))

(define (quine q)
  (valofo q '() '() q))

(define (twine prog)
  (fresh (p q)
    (== prog `(,p ,q))
    (=/= p q)
    (valofo p '() '() q)
    (valofo q '() '() p)))

(define (thine prog)
  (fresh (p q r)
    (== prog `(,p ,q ,r))
    (=/= p q)
    (=/= q r)
    (=/= p r)
    (valofo p '() '() q)
    (valofo q '() '() r)
    (valofo r '() '() p)))
