include module type of Structure_types

val dmg_modifier : Weapon.t -> struct_strength -> Pct.t
val calc_dmg : t -> Weapon.t -> int
val init : unit -> unit
val values : (string, t) Core.Hashtbl.t
val names_to_ids: (string, string) Core.Hashtbl.t
val by_name_exn: string -> t
val by_name: string -> t option
val by_id_exn: string -> t
val by_id: string -> t option