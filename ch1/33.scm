(load "../libs/init.scm")
(load "31.scm")

(define mark-leaves-with-red-depth
	(lambda (btree)
		(mark-leaves-with-red-accumulated btree 0)))

(define mark-leaves-with-red-accumulated
	(lambda (btree n)
		(cond ((leaf? btree) n)
			((eq? (contents-of btree) 'red)
				(interior-node 'red
					(mark-leaves-with-red-accumulated (lson btree) (+ n 1))
					(mark-leaves-with-red-accumulated (rson btree) (+ n 1))))
			(else
				(interior-node (contents-of btree)
					(mark-leaves-with-red-accumulated (lson btree) n)
					(mark-leaves-with-red-accumulated (rson btree) n))))))

(equal?? (mark-leaves-with-red-depth
	(interior-node 'red
		(interior-node 'bar
			(leaf 26)
			(leaf 12))
		(interior-node 'red
			(leaf 11)
			(interior-node 'quux
				(leaf 117)
				(leaf 14))))) 
'(red
	(bar 1 1)
	(red 2 (quux 2 2))))