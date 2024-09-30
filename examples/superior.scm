(define (a a b)
  (if (eq? a b)
	#f
     (if (< a b)
	#f
	#t)))
(a 10 -2)
