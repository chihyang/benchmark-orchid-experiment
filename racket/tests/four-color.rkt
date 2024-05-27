#lang racket

(provide do-australia do-australia/code do-canada do-canada/code do-america do-america/code timing-test)

(require "../mk.rkt"
         (prefix-in australia: "data/australia.rkt")
         (prefix-in canada: "data/canada.rkt")
         (prefix-in america: "data/america.rkt")
         "lists.rkt")

;;
;;
;; This part of the code implements the graph reduction idea
;; described in this paper:
;; <http://www-formal.stanford.edu/jmc/coloring/coloring.html>
;;
;;

(define (list-subtract l1 l2)
  ;; not the best algorithm
  (remove (lambda (elt) (member elt l2)) l1))

(define (graph-list-neighbours node edges)
  (define (update neighbours edge)
    ;; Using cons here assumes the graph has no redundancy,
    ;; Replace with list-insert (from mergesort) if needed.
    (cond ((equal? node (car edge))
           (cons (cadr edge) neighbours))
          ((equal? node (cadr edge))
           (cons (car edge) neighbours))
          (else neighbours)))
  (let loop ((neighbours '()) (edges edges))
    (if (null? edges)
        neighbours
        (loop (update neighbours (car edges)) (cdr edges)))))

(define (subgraph-edges nodes edges)
  ;; This lets remove edges from a graph by passing in a new
  ;; smaller list of nodes.
  (filter (lambda (edge)
            (and (member (car edge) nodes)
                 (member (cadr edge) nodes)))
          edges))

(define (graph-reduction-split nodes edges)
  (let-values
      (((nodes-a nodes-b)
        (partition (lambda (node) (> (length (graph-list-neighbours node edges)) 3))
                   nodes)))
    (let ((edges-a (subgraph-edges nodes-a edges)))
      (values nodes-a edges-a
              nodes-b (list-subtract edges edges-a)))))

(define (graph-reduction-loop nodes edges tail)
  (let-values (((nodes-a edges-a nodes-b edges-b)
                (graph-reduction-split nodes edges)))
    (cond ((null? nodes-a)
           ;; the graph is fully reduced (to the null graph)
           (cons (cons nodes-b edges-b) tail))
          ((null? nodes-b)
           ;; the graph is irreducible
           (cons (cons nodes-a edges-a) tail))
          (else
           (graph-reduction-loop nodes-a edges-a (cons (cons nodes-b edges-b) tail))))))

(define (graph-good-ordering nodes edges)
  (apply append (map car (graph-reduction-loop nodes edges '()))))

;;
;;
;; This is the minikanren part
;;
;;

(define (coloro x)
  (membero x '(red green blue yellow)))

(define (different-colors table)
  (lambda (constraint)
    (fresh (x y x-color y-color)
      (== constraint `(,x ,y))
      (assoco x table x-color)
      (assoco y table y-color)
      (=/= x-color y-color))))

;; (define (my-mapo p l i)
;;   ;; This has to be done in a depth first search!
;;   ;; (display (make-list i '-)) (newline)
;;   (conde/dfs ((== l '()))
;;       ((fresh (car cdr)
;;      (== l `(,car . ,cdr))
;;      (p car)
;;      (my-mapo p cdr (+ i 1))))))

(define (color states edges colors)
  ;; This is a simple constrained generate and test solver
  ;; The interesting part was the graph reduction preprocessing
  ;; stage.
  (fresh (table)
    ;; make a list to hold the color of each state
    (make-assoc-tableo states colors table)

    ;; make sure each color is different to neighbours
    (mapo (different-colors table) edges)

    ;; brute force search for a valid coloring
    (mapo coloro colors)))

(define (do-australia)
  (let ((nodes (graph-good-ordering australia:nodes australia:edges)))
    (display nodes)(newline)
    (runc 1 (lambda (q) (color nodes australia:edges q)))))

(define (do-australia/code)
  (let ((nodes (graph-good-ordering australia:nodes australia:edges)))
    (lambda (q) (color nodes australia:edges q))))

(define (do-canada)
  (let ((nodes (graph-good-ordering canada:nodes canada:edges)))
    (display nodes)
    (newline)
    (runc 1 (lambda (q) (color nodes canada:edges q)))))

(define (do-canada/code)
  (let ((nodes (graph-good-ordering canada:nodes canada:edges)))
    (lambda (q) (color nodes canada:edges q))))

(define (do-america)
  (let ((nodes (graph-good-ordering america:nodes america:edges)))
    (display nodes)(newline)
    (runc 2 (lambda (q) (color nodes america:edges q)))))

(define (do-america/code)
  (let ((nodes (graph-good-ordering america:nodes america:edges)))
    (lambda (q) (color nodes america:edges q))))


;; This was left around from debugging/benchmarking
(define (make-string-nodes n)
  (if (= n 0)
      '()
      (cons n (make-string-nodes (- n 1)))))

(define (make-string-edges n)
  (if (< n 2)
      '()
      (cons (list n (- n 1)) (make-string-edges (- n 1)))))

(define (timing-test n)
  (let ((t1 (current-inexact-monotonic-milliseconds)))
    (runc 1 (lambda (q) (color (make-string-nodes n) (make-string-edges n) q)))
    (- (current-inexact-monotonic-milliseconds) t1)))