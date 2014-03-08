

module equality-maker
interface
((ints : [opaque t
		 zero : t
		 succ : (t -> t)
		 pred : (t -> t)
		 is-zero : (t -> bool)])
 => [equal : (from ints take t
		   -> (from ints take t
			    -> bool))])
body
[
 equal = let ty = from ints take t
       in let z? = from ints take is-zero
       in let s = from ints take succ
       in let p = from ints take pred
       in letrec (ty -> bool) equal(x : ty) =
                                proc(y : ty)
				if z?(x) and z?(y) then #t
				else if (or (and z?(x)  (not z?(y)))
					    (and z?(y)  (not z?(x))))
				      then #f
				      else ((equal (pred x)) (pred y))
           in equal]
