include module type of Construction_types

val init : unit -> unit
val values : (string, t) Core.Hashtbl.t
val by_name_exn : string -> t
val by_name : string -> t option
val by_id_exn : string -> t
val by_id : string -> t option
