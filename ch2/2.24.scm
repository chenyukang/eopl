#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))



(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
	   (leaf-node (num)
		      (list 'leaf-node num))
	   (interior-node (key left right)
			  (list 'interior-node
				key
				(bintree-to-list left)
				(bintree-to-list right))))))


(define tree
  (interior-node 'a (leaf-node 3) (leaf-node 4)))
;;#(struct:interior-node a #(struct:leaf-node 3) #(struct:leaf-node 4))

(bintree-to-list tree)
	 
;;(interior-node a (leaf-node 3) (leaf-node 4))
