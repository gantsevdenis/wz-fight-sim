include module type of Propulsion_types

val dmg_modifier : Weapon_types.t -> t -> Pct.t
val init : unit -> unit
val values : (string, t) Core.Hashtbl.t

(** Find an object with id, or name equal to [s] *)
val find_similar: string -> t option

(** Find an object where [s] is substring of id, or name *)
val find_all_similar: string -> t list

val by_name_exn : string -> t
val by_name : string -> t option
val by_id_exn : string -> t
val by_id : string -> t option
