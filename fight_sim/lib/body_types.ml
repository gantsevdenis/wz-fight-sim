open Core

type body_size =
  | LIGHT
  | MEDIUM
  | HEAVY
  | SUPER_HEAVY

let body_size_of_string s =
  match s with
  | "LIGHT" -> LIGHT
  | "MEDIUM" -> MEDIUM
  | "HEAVY" -> HEAVY
  | "SUPER_HEAVY" -> SUPER_HEAVY
  | s -> raise (Invalid_argument s)
;;

type body_class =
  | BABAS
  | DROIDS
  | CYBORGS
  | TRANSPORTS

let body_class_of_string s =
  match s with
  | "BABAS" -> BABAS
  | "DROIDS" -> DROIDS
  | "CYBORGS" -> CYBORGS
  | "TRANSPORTS" -> TRANSPORTS
  | s -> raise (Invalid_argument s)
;;

type t =
  { armour : Weapon_types.weapon_class -> int
  ; buildPoints : int
  ; buildPower : int
  ; clazz : body_class
  ; designable : bool
  ; hitpoints : int
  ; id : string
  ; name : string
  ; powerOutput : int
  ; size : body_size
  ; weaponSlots : int
  ; weight : int
  }

