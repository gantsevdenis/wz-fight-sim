(* open Dolog *)
include Droid_types

type research_context = Research.context

let of_template ?(position = 0, 0) template rctx =
  { template
  ; exp = 0
  ; position
  ; cur_hp = Droid_template.effective_hp template rctx
  ; purpose = DROID_WEAPON
  ; has_commander = None
  }
;;

let is_susceptible_to (d : t) (w : Weapon.t) =
  match w.flags, d.template.propulsion.typ with
  | Weapon.AIR_ONLY, Propulsion.LIFT -> true
  | Weapon.AIR_ONLY, _ -> false
  | Weapon.BOTH, _ -> true
  | Weapon.GROUND, Propulsion.LIFT -> false
  | Weapon.GROUND, _ -> true
;;

let is_transport (d : t) =
  match d.template.body.clazz with
  | Body.TRANSPORTS -> true
  | _ -> false
;;

(* let is_commander (d : t) = d.template.weapons.(0).id = Weapon_uid.wrap "CommandTurret1" *)

let is_vtol (d : t) =
  match d.template.propulsion.typ with
  | Propulsion.LIFT -> not (is_transport d)
  | _ -> false
;;

(* let commandbrain_thresholds = Array.of_list [ 0; 8; 16; 32; 64; 128; 256; 512; 1024 ] *)
let commandbrain_thresholds kills =
  match kills with
  | n when n < 8 -> 0
  | n when n < 16 -> 1
  | n when n < 32 -> 2
  | n when n < 64 -> 3
  | n when n < 128 -> 4
  | n when n < 256 -> 5
  | n when n < 512 -> 6
  | n when n < 1024 -> 7
  | _ -> 8
;;

let nullbrain_thresholds kills =
  match kills with
  | n when n < 4 -> 0
  | n when n < 8 -> 1
  | n when n < 16 -> 2
  | n when n < 32 -> 3
  | n when n < 64 -> 4
  | n when n < 128 -> 5
  | n when n < 256 -> 6
  | n when n < 512 -> 7
  | _ -> 8
;;

let rank_names rank =
  match rank with
  | 0 -> "Rookie"
  | 1 -> "Green"
  | 2 -> "Trained"
  | 3 -> "Regular"
  | 4 -> "Professional"
  | 5 -> "Veteran"
  | 6 -> "Elite"
  | 7 -> "Special"
  | _ -> "Hero"
;;

let rank (d : t) =
  match d.purpose with
  | DROID_COMMAND -> commandbrain_thresholds (d.exp / 65536)
  | _ -> nullbrain_thresholds (d.exp / 65536)
;;

let effective_level (d : t) =
  match d.has_commander with
  | Some c -> max (rank d) (rank c)
  | None -> rank d
;;

(** Returns true is died *)
let apply_dmg (d : t) points =
  d.cur_hp <- d.cur_hp - points;
  if d.cur_hp > 0 then false else true
;;

let rank_modifier r = Pct.of_int (100 - (6 * r))

(* 
  Calculate dmg to a target droid; takes into account minimum dmg and droid rank  
  TODO: VTOLs (and transporters in MP) on the ground take triple damage
  TODO: dmg per second
*)
let _calc_dmg (victim : t) (w1 : Weapon.t) ~agg_ctx ~target_ctx =
  (* ------- adjust to target's propulsion *)
  let dmg = Droid_template.calc_dmg victim.template w1 agg_ctx in
  (* ------- adjust to target's experience rank*)
  let armour = Droid_template.effective_armour victim.template w1 target_ctx in
  let apply_exp = Pct.scale_by (rank_modifier (effective_level victim)) dmg in
  let apply_weapon_min = Pct.scale_by w1.minimumDamage apply_exp in
  let out = max (apply_exp - armour) apply_weapon_min in
  Dolog.Log.debug
    "template_dmg;actual;rank; %i %i %i %i %s"
    dmg
    apply_exp
    apply_weapon_min
    out
    (Pct.to_string w1.minimumDamage);
  out
;;

let _calc_dmg_periodical (victim : t) w1 ~agg_ctx ~target_ctx =
  let dmg = Droid_template.calc_dmg_periodical victim.template w1 agg_ctx in
  let armour = Droid_template.effective_armour_periodical victim.template w1 target_ctx in
  let apply_exp = Pct.scale_by (rank_modifier (effective_level victim)) dmg in
  let apply_weapon_min = Pct.scale_by w1.minimumDamage apply_exp in
  let out = max (apply_exp - armour) apply_weapon_min in
  Dolog.Log.debug
    "template_dmg;actual;rank; %i %i %i %i %s"
    dmg
    apply_exp
    apply_weapon_min
    out
    (Pct.to_string w1.minimumDamage);
  out
;;

(** Returned damage takes into account:
    - target's propulsion modifier
    - weapon's research level (=weapon bonus)
    - target's experience bonus (=weapon malus)
    - target's armour bonus (=weapon malus) *)
let calc_dmg (victim : t) (agg_w : Weapon.t) ~agg_ctx ~target_ctx = max (_calc_dmg victim agg_w ~agg_ctx ~target_ctx) 1

let calc_dmg_periodical (victim : t) agg_w ~agg_ctx ~target_ctx =
  max (_calc_dmg_periodical victim agg_w ~agg_ctx ~target_ctx) 1
;;
