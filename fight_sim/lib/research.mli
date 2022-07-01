include module type of Research_types

type t
type context

val add_points : t -> context -> int -> bool
val effective_research_points : context -> has_module:bool -> int
val calc_dependencies : t -> t list
val compare : t -> t -> int
val empty : unit -> context
val fold_upgrades : context -> ('a -> Upgrades.t -> 'a) -> 'a -> 'a
val get_parents : t -> t list option
val get_upgrades : t -> Upgrades.t list option
val get_children : t -> t list option
val get_children_exn : t -> t list
val grant_all_upgrades : context -> unit

(** convert string id to positional index. *)
val of_string_exn : string -> t
val of_string : string -> t option
val to_string : t -> string

(** Grants a particular research immediately*)
val grant_research : t -> context -> unit

(** Grant a particular research immediately, and also grant all prerequisites *)
val grant_upto_research : t -> context -> unit

val grant_upto_research_opt : t option -> context -> unit
val grant_research_exn : t -> context -> unit
val hash : t -> int
val human_name_exn : t -> string
val is_completed : t -> context -> bool
val is_research_upgrade : t -> bool
val is_module_upgrade : t -> bool
val is_possible : t -> context -> bool

(** cost in power *)
val cost : t -> int
val cost_points : t -> int

(** Iterate over completed upgrades *)
val iter_upgrades : context -> (Upgrades.t -> unit) -> unit

val iter_research : (t -> unit) -> unit
val fold_research : f:('a -> t -> 'a) -> init:'a -> 'a
val needed : t -> context -> int

(** Given a structure id, what research has enabled it? *)
val structure_enabled : string -> t option

(** Given a component id, what research has enabled it? *)
val component_enabled : string -> t option
val structure_enabled_exn : string -> t
val component_enabled_exn : string -> t

(** Given a research, what structures it has enabled? *)
val enabled_structures : t -> string list

(** Given a research, what components it has enabled? *)
val enabled_components : t -> string list
val equal : t -> t -> bool
val contains : t -> t list -> bool
val init : unit -> unit