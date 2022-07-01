include module type of Droid_template_types

val total_cost : t -> int
val total_weight : t -> int

(** used to calculate time to build*)
val total_build_points : t -> int

(** Returns max possible hitpoints. 
    Takes into account research level *)
val effective_hp : t -> Research.context -> int

val effective_armour_kinetic : t -> Research.context -> int
val effective_armour_heat : t -> Research.context -> int
val effective_armour : t -> Weapon.t -> Research.context -> int
val effective_armour_periodical : t -> Weapon.t -> Research.context -> int

(** Returned damage takes into account:
    - target's propulsion modifier
    - weapon's research level (=weapon bonus) *)
val calc_dmg : t -> Weapon.t -> Research.context -> int

val calc_dmg_periodical : t -> Weapon.t -> Research.context -> int

(** calls Research to grant all technologies needed for this template *)
val grant_all_for : t -> Research.context -> unit
(* val calc_expected_dmg : t -> Weapon.t -> Research.context -> Weapon.dist_spec_t -> int *)

(* val calc_expected_dmg_periodical : t -> Weapon.t -> Research.context -> Weapon.dist_spec_t -> int *)
val of_components : Propulsion.t -> Weapon.t -> Body.t -> t

val research_needed : t -> Research.t list

(** try to parse string like "light cannon viper half-tacks" into a template *)
val of_string : string -> t option
val of_string_exn: string -> t
val generic_name : t -> string