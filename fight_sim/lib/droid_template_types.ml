type t =
  { id : string
  ; name : string
  ; propulsion : Propulsion.t
  ; weapon : Weapon.t (** technically, it's possible to have >1 weapon, but we don't care about that much *)
  ; body : Body.t
  }
