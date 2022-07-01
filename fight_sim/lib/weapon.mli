include module type of Weapon_types
type dist_spec_t =
  | Long
  | Short
  | Best
val expected_to_string : expected_t -> string
val effective_dmg : t -> Research.context -> int
val effective_dmg_periodical: t -> Research.context -> int
val effective_reload_time : t -> Research.context -> int
val effective_firepause : t -> Research.context -> int
val effective_long_hitchance : t -> Research.context -> Pct.t
val effective_short_hitchance : t -> Research.context -> Pct.t

(** what is the chance of hitting a target at that distance? *)
val effective_hitchance_at_dist : t -> Research.context -> int -> Pct.t

(* val effective_dmg_at_dist : t -> Research.context -> int -> Pct.t *)
val expected_dmg_short : t -> Research.context -> int
val expected_dmg_long : t -> Research.context -> int
val effective_rof: t -> Research.context -> int
val range_best : t -> int
val range_dmg_best : t -> int * Pct.t
val best_hit : t -> Pct.t
val normalized_dmg : t -> int

(** given a [target] to reach, and [expected_dmg] how many salvos/time this weapon needs to achieve it?
    [expected_dmg] must be provided, so that we don't depend on "Droid*" logic
    with regard to research level of our victim *)
(* val expected_salvos_to_reach : t-> Research.context -> expected_dmg:int -> target:int -> expected_t *)

(* val normalized_expected_dmg_short : t -> int *)

(* val normalized_expected_dmg_long : t -> int

val normalized_expected_dmg_best : t -> int *)
val filter_value_of_weapon_subclass : weapon_subclass -> Upgrades.filter_value
val sum_upgrades_dmg : t -> Research.context -> Pct.t
val sum_upgrades_reload_time : t -> Research.context -> Pct.t
val sum_upgrades_firepause : t -> Research.context -> Pct.t
val sum_upgrades_long_hitchance : t -> Research.context -> Pct.t
val sum_upgrades_short_hitchance : t -> Research.context -> Pct.t
val is_direct : t -> bool
(* val weapon_rof : t -> int *)
val init : unit -> unit
(* find all research ids which would provide upgrades for this weapon *)
(* val find_all_upgrades : t -> Research.t list *)
val values : (string, t) Core.Hashtbl.t

(** Find an object with id, or name equal to [s] *)
val find_similar: string -> t option

(** Find an object where [s] is substring of id, or name *)
val find_all_similar: string -> t list

val by_name_exn: string -> t
val by_name: string -> t option
val by_id_exn: string -> t
val by_id: string -> t option