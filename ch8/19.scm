

;; finish module procedure with to-two

module from-int-maker
interface
((ints : [opaque t
		 zero : t
		 succ : (t -> t)
		 pred : (t -> t)
		 is-zero : (t -> bool)])
 => [from-int : (int -> from ints take t)])
body
module-proc (ints : [opaque t
			    zero : t
			    succ : (t -> t)
			    pred : (t -> t)
			    is-zero : (t -> bool)])
[to-int
   = let z? = from ints take is-zero
   in let p = from ints take pred
   in letrec int to-int (x : from ints take t)
   in to-int

to-two
       = (from ints take succ
	       (from ints take succ
		     from ints take zero))]
