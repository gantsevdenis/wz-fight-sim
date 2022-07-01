(** %, Centered at 100 *)

type t = private int

val of_int : int -> t

(** -> x * base / 100 *)
val scale_by : t -> int -> int

(** some wierd logic in research.cpp:823 requires this special division ... *)
val scale_by_iceil : t -> int -> int

val to_string : t -> string
val ( + ) : t -> t -> t
val ( > ) : t -> t -> bool
val ( < ) : t -> t -> bool
val cent : t
val ( = ) : t -> t -> bool