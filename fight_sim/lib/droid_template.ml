include Droid_template_types
open Core

let generic_name x =
  Printf.sprintf "%s %s %s" x.propulsion.name x.body.name x.weapon.name

let with_name x name = {x with name}

let of_components prop weapon body =
  let tmp = {id= ""; name= ""; propulsion= prop; weapon; body} in
  with_name tmp (generic_name tmp)

let of_string s =
  let parts = String.split ~on:' ' s in
  let parts = List.remove_consecutive_duplicates parts ~equal:String.equal in
  let parts = List.filter ~f:(fun el -> not (String.is_empty el)) parts in
  (* rebuild initial string without repeating blancks *)
  let s = String.concat ~sep:" " parts in
  let body : Body.t option ref = ref None in
  let prop : Propulsion.t option ref = ref None in
  let found_idx = ref [] in
  (* parts= ["light"; "cannon"; "viper"; "half-tracks"; ""] *)
  List.iteri
    ~f:(fun i el ->
      (* try to find what it is ... *)
      match (Body.find_similar el, Propulsion.find_similar el) with
      | Some b, None ->
          body := Some b ;
          found_idx := i :: !found_idx
      | None, Some p ->
          prop := Some p ;
          found_idx := i :: !found_idx
      | _ ->
          () )
    parts ;
  match (!prop, !body) with
  | None, None | None, Some _ | Some _, None ->
      None
  | Some p, Some b -> (
      (* remove body and propulsion: what's left is weapon name *)
      let non_parsed =
        List.foldi
          ~f:(fun i acc el ->
            (* also filter out empty leftovers *)
            if List.mem !found_idx i ~equal:Int.equal then acc else el :: acc )
          ~init:[] parts
      in
      let weapon_name = String.concat ~sep:" " (List.rev non_parsed) in
      Dolog.Log.debug "'%s' - '%s'" weapon_name s ;
      assert (String.is_substring ~substring:weapon_name s) ;
      match Weapon.find_similar weapon_name with
      | None ->
          None
      | Some w ->
          Some (of_components p w b) )

let of_string_exn s =
  match of_string s with None -> raise (Invalid_argument s) | Some t -> t

let grant_all_for templ ctx =
  Research.(
    grant_upto_research (component_enabled_exn templ.weapon.id) ctx ;
    grant_upto_research (component_enabled_exn templ.propulsion.id) ctx ;
    grant_upto_research (component_enabled_exn templ.body.id) ctx)

let research_needed (templ : t) =
  let open Research in
  let d_w = calc_dependencies (component_enabled_exn templ.weapon.id) in
  let d_p = calc_dependencies (component_enabled_exn templ.propulsion.id) in
  let d_b = calc_dependencies (component_enabled_exn templ.body.id) in
  List.dedup_and_sort (d_w @ d_p @ d_b) ~compare:Research.compare

let total_cost (templ : t) =
  Pct.scale_by templ.propulsion.buildPower templ.body.buildPower
  + templ.body.buildPower + templ.weapon.buildPower

let total_build_points (templ : t) =
  let p1 = Pct.scale_by templ.propulsion.buildPoints templ.body.buildPoints in
  Dolog.Log.debug "buildPoints %s: %i %i %i" templ.name
    (Pct.scale_by templ.propulsion.buildPoints 100)
    templ.body.buildPoints templ.weapon.buildPoints ;
  p1 + templ.body.buildPoints + templ.weapon.buildPoints

let total_weight (templ : t) =
  templ.body.weight
  + Pct.scale_by templ.propulsion.weightPct templ.body.weight
  + templ.weapon.weight

let _sum_upgrades param fv rctx =
  Research.fold_upgrades rctx
    (fun acc (x : Upgrades.t) ->
      match x with
      | Upgrade (CLS_BODY, x, Some (FP_BODYCLASS, fv_), n)
        when Poly.(x = param && fv_ = fv) ->
          Pct.(acc + n)
      | _ ->
          acc )
    Pct.cent

let effective_armour_kinetic dt ctx =
  let base = dt.body.armour KINETIC in
  let open Upgrades in
  let fv =
    match dt.propulsion.typ with LEGGED -> FV_CYBORGS | _ -> FV_DROIDS
  in
  let armour_up_kinetic = _sum_upgrades P_ARMOUR fv ctx in
  Pct.scale_by armour_up_kinetic base

let effective_armour_heat dt ctx =
  let base = dt.body.armour HEAT in
  let open Upgrades in
  let fv =
    match dt.propulsion.typ with LEGGED -> FV_CYBORGS | _ -> FV_DROIDS
  in
  let armour_up_heat = _sum_upgrades P_THERMAL fv ctx in
  Pct.scale_by armour_up_heat base

let effective_armour dt (w : Weapon.t) ctx =
  match w.dmgClass with
  | HEAT ->
      effective_armour_heat dt ctx
  | KINETIC ->
      effective_armour_kinetic dt ctx

let effective_armour_periodical dt (w : Weapon.t) ctx =
  match w.periodicalDmgClass with
  | HEAT ->
      effective_armour_heat dt ctx
  | KINETIC ->
      effective_armour_kinetic dt ctx

let effective_hp dt rctx =
  let body_hp = dt.body.hitpoints in
  let fv =
    let open Upgrades in
    match dt.propulsion.typ with LEGGED -> FV_CYBORGS | _ -> FV_DROIDS
  in
  let hpUp = _sum_upgrades P_HIT_POINTS fv rctx in
  let hpUpPct = _sum_upgrades P_HIT_POINT_PCT fv rctx in
  let weapons_hp = dt.weapon.hitpoints in
  let base_hp = Pct.scale_by hpUp body_hp in
  let prop_hp = Pct.(scale_by dt.propulsion.hitpointPctOfBody base_hp) in
  (* Dolog.Log.debug "%i %i %i %i %s%% %s%%" body_hp base_hp prop_hp weapons_hp
     (Pct.to_string hpUp) (Pct.to_string hpUpPct) ; *)
  let total = base_hp + prop_hp + weapons_hp in
  Pct.scale_by hpUpPct total

(* assumes body modifier is 100 *)
let calc_dmg victim (w1 : Weapon.t) ctx =
  assert (w1.damage > 0) ;
  let modifier_prop = Propulsion.dmg_modifier w1 victim.propulsion in
  let effective_dmg = Weapon.effective_dmg w1 ctx in
  max Pct.(scale_by modifier_prop effective_dmg) 1

let calc_dmg_periodical victim (w1 : Weapon.t) ctx =
  assert (w1.damage > 0) ;
  let modifier_prop = Propulsion.dmg_modifier_periodical w1 victim.propulsion in
  let effective_dmg = Weapon.effective_dmg_periodical w1 ctx in
  max Pct.(scale_by modifier_prop effective_dmg) 1

(* let calc_expected_dmg d (w : Weapon.t) (rctx : Research.context) (dist_spec : Weapon.dist_spec_t) =
     assert (w.damage > 0);
     let effective_dmg = Weapon.effective_dmg w rctx in
     let modifier_prop = Propulsion.dmg_modifier w d.propulsion in
     let after_expectation =
       match dist_spec with
       | Short -> Pct.(scale_by w.shortHit (scale_by modifier_prop effective_dmg))
       | Long -> Pct.(scale_by w.longHit (scale_by modifier_prop effective_dmg))
       | _ -> raise (Invalid_argument "unknown option")
     in
     max after_expectation 1
   ;; *)
