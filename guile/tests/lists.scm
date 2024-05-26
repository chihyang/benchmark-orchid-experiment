(use-modules (minikanren language)
	     (minikanren examples lists))
(use-modules (srfi srfi-64))

(test-begin "lists membero")
(test-equal (run* (lambda (q)
		    (membero q '(a b c))))
  '((a where)
    (b where)
    (c where)))
(test-end "lists membero")