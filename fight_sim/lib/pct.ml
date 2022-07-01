type t = int

let scale_by (x : t) (base : int) = x * base / 100
let scale_by_iceil x base = Utils.idiv (x * base) 100
let of_int (x : int) : t = x
let ( + ) a b = a + b - 100
let to_string x = Int.to_string x
let ( > ) (a : t) b = a > b
let ( < ) (a : t) b = a < b
let ( = ) = ( = )
let cent = 100
