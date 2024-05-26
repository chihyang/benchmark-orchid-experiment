(define-module (minikanren run)
  #:export (run^ run* runi runc ==))
(use-modules (minikanren binary-trie)
	     (minikanren streams)
	     (minikanren unification)
             (minikanren disequality)
	     (minikanren kanren))
(use-modules (srfi srfi-11))

(define (print s) (display s) (newline))

;;
;; Reifying terms and kanren states
;;

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (reify-s v s)
  ;; Given a term and substitution this extends
  ;; the substitution with nice names for each
  ;; fresh variable in v
  (let ((v (walk v s)))
    (cond
     ((var? v) (let ((n (reify-name (trie-size s))))
                 (trie-insert s (var->int v) n)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-term t s)
  ;; To reify a term:
  ;;
  ;; First use walk* so that we have a flat term
  ;; containing only fresh variables
  ;;
  ;; Then extend our substitution with good names
  ;; for all those variables
  ;;
  ;; walk* it again to reify all the fresh variables
  ;; in the term itself.
  ;;
  ;; Doing it in two steps like this means we use up
  ;; names for any variables that aren't in the final
  ;; term
  (let ((v (walk* t s)))
    (walk* v (reify-s v '()))))

(define (reify-kanren k)
  ;; The query variable will always be the very first
  ;; one so reify (var 0) along with all the constraints
  ;; or extra conditions that might need to be displayed
  (define (make-=/= eq) `(=/= ,(car eq) ,(cdr eq)))
  (reify-term `(,(var 0) where .
                ,(append (map (lambda (d)
                                `(or . ,(map make-=/= d)))
                              (disequality-store k))))
              (substitution k)))

;;
;; Running a goal
;;

(define (run^ n g)
  ;; Compute up to a set limit of results
  (map reify-kanren (take n ((call/fresh g) initial-kanren))))

(define (run* g)
  ;; Compute every result
  (map reify-kanren (take-all ((call/fresh g) initial-kanren))))

(define (runi g)
  ;; This version of run returns one result from the
  ;; stream at a time interactively asking if you
  ;; want more or not
  (let (($ ((call/fresh g) initial-kanren)))
    (let loop (($ (pull $)))
      (if (null? $)
          (print 'thats-all!)
          (begin (print (reify-kanren (car $)))
                 (print '(another? y/n))
                 (case (read)
                   ((y yes) (loop (pull (cdr $))))
                   (else (print 'bye!))))))))

;;
;; Unification
;;

(define (== u v)
  (lambda (k)
    (let-values (((s p) (unify/prefix u v (substitution k))))
      (if s
	  (normalize-disequality-store
           (modified-substitution (lambda (_) s) k))
	  mzero))))

(define (product$ $1 $2)
  (cond
   ((null? $2) '())
   ((procedure? $2) (product$ $1 ($2)))
   (else (mplus (merge$ (car $2) $1) (product$ $1 (cdr $2))))))

(define (merge$ k $)
  (cond
   ((null? $) '())
   ((pair? $)
    (let ((k^ (product-k k (car $))))
      (if k^
          (cons k^ (merge$ k (cdr $)))
          (merge$ k (cdr $)))))
   (else (λ () (merge$ k ($))))))

(define (product-k k1 k2)
  (let* ((c1 (counter k1))
         (s1 (substitution k1))
         (d1 (disequality-store k1))
         (k2 (modified-counter
              (lambda (c2)
                (if (< c1 c2) c2 c1))
              (modified-disequality-store (lambda (d2) (append d1 d2)) k2)))
         (s2 (substitution k2))
         (s2^ (product-s (substitution->pairs s1) s2)))
    (if s2^
        (let ((s2 (normalize-disequality-store
                   (modified-substitution (lambda (_) s2^) k2))))
          (if (null? s2) #f (car s2)))
        #f)))

(define (substitution->pairs s)
  (map (lambda (v) (cons (var (car v)) (cdr v))) (binary-trie->assoc-list s)))

(define (product-s s1 s2)
  (cond
   ((null? s1) s2)
   (else
    (let ((s2 (unify (caar s1) (cdar s1) s2)))
      (if s2
          (product-s (cdr s1) s2)
          #f)))))

(define (take-rest n $ acc)
  (cond
   ((or (zero? n) (null? $)) (cons acc $))
   ((pair? $)
    (take-rest (- n 1) (cdr $) (cons (car $) acc)))
   (else (take-rest n ($) acc))))

(define (runc n g)
  (let ((q (var (counter initial-kanren))))
    (let ((result (take-rest n ((g q) (modified-counter 1+ initial-kanren)) '())))
      (cons (map reify-kanren (car result))
            (lambda (q^)
              (let ((g^ (== q^ q)))
                (lambda (k)
                  (product$ (cdr result) (g^ k)))))))))
