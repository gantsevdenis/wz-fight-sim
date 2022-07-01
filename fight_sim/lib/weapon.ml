open Core
open Utils
include Weapon_types

type dist_spec_t = Long | Short | Best

module StrTable = Hashtbl.Make (String)

let values : t StrTable.t = StrTable.create ()

let names_to_ids = StrTable.create ()

let by_name_exn s = Utils.by_name_exn values names_to_ids s

let by_name s = Utils.by_name values names_to_ids s

let by_id_exn s = Utils.by_id_exn values s

let by_id s = Utils.by_id values s

let find_similar s =
  Utils.find_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s

let find_all_similar s =
  Utils.find_all_similar ~values ~names_to_ids ~by_id_exn ~by_name_exn s

let best_hit w = if Pct.(w.longHit > w.shortHit) then w.longHit else w.shortHit

let range_dmg_best w =
  if Pct.(w.longHit > w.shortHit) then (w.longRange, w.longHit)
  else (w.shortRange, w.shortHit)

let range_best w =
  if Pct.(w.longHit > w.shortHit) then w.longRange else w.shortRange

let normalized_dmg w = w.damage / max w.firePause 1
(* let normalized_expected_dmg_short w = expected_dmg_short w / max w.firePause 1
   let normalized_expected_dmg_long w = expected_dmg_long w / max w.firePause 1
   let normalized_expected_dmg_best w = expected_dmg_best w / max w.firePause 1 *)

let filter_value_of_weapon_subclass scl =
  let open Upgrades in
  match scl with
  | AA_GUN ->
      FV_AA_GUN
  | BOMB ->
      FV_BOMB
  | CANNON ->
      FV_CANNON
  | COMMAND ->
      FV_COMMANDBRAIN01
  | ELECTRONIC ->
      assert false (* doesn't happen *)
  | EMP ->
      assert false (* doesn't happen *)
  | ENERGY ->
      FV_ENERGY
  | FLAME ->
      FV_FLAME
  | GAUSS ->
      FV_GAUSS
  | HOWITZERS ->
      FV_HOWITZERS
  | LAS_SAT ->
      assert false (* doesn't happen *)
  | MACHINE_GUN ->
      FV_MACHINE
  | MISSILE ->
      FV_MISSILE
  | MORTARS ->
      FV_MORTARS
  | ROCKET ->
      FV_ROCKET

(* TODO: can we have more than FP_IMPACTCLASS for weapons? *)
let _sum_upgrades_weapon_impactclass param subClass rctx =
  Research.fold_upgrades rctx
    (fun acc up ->
      let open Upgrades in
      match up with
      | Upgrade (CLS_WEAPON, arg_param, Some (FP_IMPACTCLASS, arg_fv), n)
      (* use polymorphic comparaison for convenience s*)
        when Poly.(arg_fv = filter_value_of subClass && arg_param = param) ->
          Pct.(acc + n)
      | _ ->
          acc )
    Pct.cent

let sum_upgrades_dmg (w : t) rctx =
  _sum_upgrades_weapon_impactclass P_DAMAGE w.dmgSubClass rctx

let sum_upgrades_dmg_periodical w rctx =
  _sum_upgrades_weapon_impactclass P_REPEAT_DAMAGE w.periodicalDmgSubClass rctx

let sum_upgrades_reload_time (w : t) rctx =
  _sum_upgrades_weapon_impactclass P_RELOAD_TIME w.dmgSubClass rctx

let sum_upgrades_firepause (w : t) rctx =
  _sum_upgrades_weapon_impactclass P_FIRE_PAUSE w.dmgSubClass rctx

let sum_upgrades_long_hitchance (w : t) rctx =
  _sum_upgrades_weapon_impactclass P_HIT_CHANCE w.dmgSubClass rctx

let sum_upgrades_short_hitchance (w : t) rctx =
  _sum_upgrades_weapon_impactclass P_SHORT_HIT_CHANCE w.dmgSubClass rctx

let effective_dmg (w1 : t) rctx1 =
  (* some wierd logic in research.cpp:823 requires this special division ... *)
  Pct.scale_by_iceil (sum_upgrades_dmg w1 rctx1) w1.damage

let expected_dmg_short w ctx = Pct.scale_by w.shortHit (effective_dmg w ctx)

let expected_dmg_long w ctx = Pct.scale_by w.longHit (effective_dmg w ctx)

(* source: stats.cpp: weaponPeriodicalDamage; projectile.cpp: proj_checkPeriodicalDamage*)
let effective_dmg_periodical (w1 : t) rctx =
  Pct.scale_by_iceil (sum_upgrades_dmg_periodical w1 rctx) w1.periodicalDmg

let effective_firepause (w1 : t) rctx1 =
  Pct.scale_by_iceil (sum_upgrades_firepause w1 rctx1) w1.firePause

let effective_short_hitchance (w1 : t) rctx1 =
  Pct.(sum_upgrades_short_hitchance w1 rctx1 + w1.shortHit)

let effective_long_hitchance (w1 : t) rctx1 =
  Pct.(sum_upgrades_long_hitchance w1 rctx1 + w1.longHit)

let effective_reload_time (w1 : t) rctx1 =
  max
    (Pct.scale_by_iceil (sum_upgrades_reload_time w1 rctx1) w1.reloadTime)
    (effective_firepause w1 rctx1)

let effective_hitchance_at_dist (w1 : t) rctx1 dist =
  if dist < w1.minRange then Pct.of_int 0
  else if dist <= w1.shortRange then effective_short_hitchance w1 rctx1
  else if dist <= w1.longRange then effective_long_hitchance w1 rctx1
  else Pct.of_int 0

let effective_rof w ctx =
  if w.numRounds > 0 then
    (* Rounds per salvo multiplied with the number of salvos per minute *)
    w.numRounds * 60 * 10 / effective_reload_time w ctx
  else
    let rof = effective_firepause w ctx in
    if rof > 0 then 60 * 10 / rof else rof

let is_direct w =
  match w.movement with DIRECT -> true | HOMING_DIRECT -> true | _ -> false

let conv_effect s =
  match s with
  | "ALL ROUNDER" ->
      "ALL_ROUNDER"
  | "ANTI PERSONNEL" ->
      "ANTI_PERSONNEL"
  | "ANTI TANK" ->
      "ANTI_TANK"
  | "ARTILLERY ROUND" ->
      "ARTILLERY_ROUND"
  | "BUNKER BUSTER" ->
      "BUNKER_BUSTER"
  | "FLAMER" ->
      "FLAMER"
  | _ ->
      assert false

let conv_subclass s =
  match s with "A-A GUN" -> "AA_GUN" | "MACHINE GUN" -> "MACHINE_GUN" | s -> s

let conv_flags s =
  match s with "AirOnly" -> "AIR_ONLY" | "ShootAir" -> "BOTH" | _ -> "GROUND"

let conv_movement s =
  match s with
  | "HOMING-DIRECT" ->
      "HOMING_DIRECT"
  | "HOMING-INDIRECT" ->
      "HOMING_INDIRECT"
  | s ->
      s

let init () =
  load_iter_data "weapons.json" (fun k v ->
      let buildPoints = get_int "buildPoints" v in
      let buildPower = get_int "buildPower" v in
      let damage = get_int "damage" v in
      let designable = get_int "designable" v = 1 in
      let dmgClass_s = get_string "weaponClass" v in
      let dmgClass = weapon_class_of_string dmgClass_s in
      let dmgSubclass_s = conv_subclass (get_string "weaponSubClass" v) in
      let dmgSubClass = weapon_subclass_of_string dmgSubclass_s in
      let effect_s = conv_effect (get_string "weaponEffect" v) in
      let effect = weapon_effect_of_string effect_s in
      let name = get_string "name" v in
      (* MG= 100=normal, Plasmite Bomb= 400, Ground Shaker=350*)
      let effectSize = Pct.of_int (get_int ~d:100 "effectSize" v) in
      let flags = weapon_flags_of_string (conv_flags (get_string "flags" v)) in
      let fireOnMove = get_int "fireOnMove" v = 1 in
      (* A-T Missile flightSpeed=1350, heavy cannon 1500 *)
      let flightSpeed = get_int "flightSpeed" v in
      let hitpoints = get_int "hitpoints" v in
      let longHit = Pct.of_int (get_int "longHit" v) in
      let longRange = get_int "longRange" v in
      let minimumDamage = Pct.of_int (get_int "minimumDamage" v) in
      let minRange = get_int "minRange" v in
      let movement =
        weapon_movement_of_string (conv_movement (get_string "movement" v))
      in
      let numAttackRuns = get_int "numAttackRuns" v in
      let numExplosions = get_int "numExplosions" v in
      (* rounds per salvo, A-T missile has 2 rounds, wait firePause between each; then wait reloadTime. *)
      let numRounds = get_int ~d:1 "numRounds" v in
      let periodicalDmg = get_int "periodicalDamage" v in
      let periodicalDmgRadius = get_int "periodicalDamageRadius" v in
      let periodicalDmgTime = get_int "periodicalDamageTime" v in
      let periodicalDmgClass =
        weapon_class_of_string
          (get_string ~d:dmgClass_s "periodicalDamageWeaponClass" v)
      in
      let periodicalDmgEffect =
        weapon_effect_of_string
          (get_string ~d:effect_s "periodicalDamageWeaponEffect" v)
      in
      let periodicalDmgSubClass =
        weapon_subclass_of_string
          (get_string ~d:dmgSubclass_s "periodicalDamageWeaponSubClass" v)
      in
      let penetrate = get_int "penetrate" v = 1 in
      let radius = get_int "radius" v in
      let radiusDamage = get_int "radiusDamage" v in
      let radiusLife = get_int "radiusLife" v in
      let recoilValue = get_int "recoilValue" v in
      (* how much gameTime to wait before next shot? *)
      let firePause = get_int "firePause" v in
      (* A-T missile reloadTime=100, but firePause 2
         default is the same as firePause *)
      let reloadTime =
        match get_int "reloadTime" v with 0 -> firePause | s -> s
      in
      let rotate = get_int "rotate" v in
      let shortHit = Pct.of_int (get_int "shortHit" v) in
      let shortRange = get_int "shortRange" v in
      let weight = get_int "weight" v in
      Dolog.Log.debug "adding weapon '%s' - '%s'" k name ;
      Hashtbl.add_exn names_to_ids ~key:name ~data:k ;
      Hashtbl.add_exn values ~key:k
        ~data:
          { buildPoints
          ; buildPower
          ; damage
          ; designable
          ; dmgClass
          ; dmgSubClass
          ; effect
          ; effectSize
          ; firePause
          ; fireOnMove
          ; flags
          ; flightSpeed
          ; hitpoints
          ; id= k
          ; longHit
          ; longRange
          ; minimumDamage
          ; minRange
          ; movement
          ; name
          ; numAttackRuns
          ; numExplosions
          ; numRounds
          ; penetrate
          ; periodicalDmg
          ; periodicalDmgRadius
          ; periodicalDmgTime
          ; periodicalDmgClass
          ; periodicalDmgEffect
          ; periodicalDmgSubClass
          ; radius
          ; radiusDamage
          ; radiusLife
          ; recoilValue
          ; reloadTime
          ; rotate
          ; shortHit
          ; shortRange
          ; weight } )

let expected_to_string (exp : expected_t) =
  Printf.sprintf "%i;%i" exp.salvos exp.sec

(** TODO: find all research which once completed, would upgrade a given weapon *)
(* let find_all_upgrades w ctx =
     Research.(
       fold_research
         ~f:(fun acc r ->
           match get_upgrades r with
           | None -> acc
           | Some ups ->
             List.(
               append
                 acc
                 (fold_left
                    ~f:(fun acc up ->
                      let open Upgrades in
                      let open Poly in
                      match up with
                      | Upgrade (CLS_WEAPON, P_DAMAGE, Some (FP_IMPACTCLASS, fv), _)
                        when fv = filter_value_of w.dmgSubClass -> up :: acc
                      | _ -> assert false)
                    ~init:[]
                    ups)))
         ~init:[])
   ;; *)
