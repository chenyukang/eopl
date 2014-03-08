
;; finish module procedure,

module sum-prod-maker
interface
((ints : [opaque t
                 zero : t
                 => [plus
                     succ : (t -> t)
                     pred : (t -> t)
                     is-zero : (t -> bool)])
 => [plus : (from ints take t
		  -> (from ints take t
			   -> from ints take t))

       times : (from ints take t
                     -> (from ints take t
			      -> from ints take t))])
       body
       [plus  = let ty = from ints take t
	      in let z? = from ints take is-zero
	      in let p = from ints take pred
	      in let s = from ints take succ
	      in letrec ty sum-proc (x: ty) =
	             proc(y : ty)
		     if z?(x) then y else
              		     ((sum-proc (p x)) (s y))
		     in sum-proc

      times =  let ty = from ints take t
               in let z? = from ints take is-zero
	       in let p = from ints take pred
	       in let s = from ints take succ
	       in letrec ty times-proc (x : ty) =
	                  proc(y : ty)
			  if z?(p x) then y else
			  ((times-proc (p x)) ((plus y) y))])
