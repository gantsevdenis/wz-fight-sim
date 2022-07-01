type run_result =
  { long_min_upgrades: Weapon.expected_t
        (* ; long_all_upgrades : Weapon.expected_t option *)
  ; short_min_upgrades: Weapon.expected_t
        (* ; short_all_upgrades : Weapon.expected_t option *) }

let printf = Printf.printf

let template_info s =
  match Droid_template.of_string s with
  | None ->
      Dolog.Log.error "'%s' is not recognized as a valid template" s ;
      exit 1
  | Some templ ->
      let ctx = Research.empty () in
      let ctx_full = Research.empty () in
      Research.grant_all_upgrades ctx_full ;
      Droid_template.(
        grant_all_for templ ctx ;
        printf "armour_heat: %i -> %i\n"
          (effective_armour_heat templ ctx)
          (effective_armour_heat templ ctx_full) ;
        printf "armour_kinetic: %i -> %i\n"
          (effective_armour_kinetic templ ctx)
          (effective_armour_kinetic templ ctx_full) ;
        printf "hitpoints: %i -> %i\n" (effective_hp templ ctx)
          (effective_hp templ ctx_full) ;
        printf "damage: %i -> %i\n"
          (Weapon.effective_dmg templ.weapon ctx)
          (Weapon.effective_dmg templ.weapon ctx_full) ;
        printf "expected damage short: %i -> %i\n"
          (Weapon.expected_dmg_short templ.weapon ctx)
          (Weapon.expected_dmg_short templ.weapon ctx_full) ;
        printf "expected damage long: %i -> %i\n"
          (Weapon.expected_dmg_long templ.weapon ctx)
          (Weapon.expected_dmg_long templ.weapon ctx_full) ;
        printf "damage periodical: %i -> %i\n"
          (Weapon.effective_dmg_periodical templ.weapon ctx)
          (Weapon.effective_dmg_periodical templ.weapon ctx_full) ;
        printf "rate of fire: %i -> %i \n"
          (Weapon.effective_rof templ.weapon ctx)
          (Weapon.effective_rof templ.weapon ctx_full) ;
        printf "reload: %i -> %i\n"
          (Weapon.effective_reload_time templ.weapon ctx)
          (Weapon.effective_reload_time templ.weapon ctx_full) ;
        printf "firepause: %i -> %i\n"
          (Weapon.effective_firepause templ.weapon ctx)
          (Weapon.effective_firepause templ.weapon ctx_full))

let print_dependencies s =
  let result_opt =
    match Research.of_string s with
    | None -> (
      (* maybe user specified a template ? *)
      match Droid_template.of_string s with
      | None ->
          Dolog.Log.error
            "%s: is not a valid research id, neither is recognised as a \
             template"
            s ;
          None
      | Some templ ->
          let deps = Droid_template.research_needed templ in
          let result = Lab.simulate_for_all deps in
          Some result )
    | Some rid ->
        let result = Lab.simulate_for_one rid in
        Some result
  in
  match result_opt with
  | None ->
      ()
  | Some result ->
      let by_start_time =
        List.sort
          (fun (_, when_l) (_, when_r) -> Int.compare when_l when_r)
          result.when_started
      in
      List.iter
        (fun (rid, when_) ->
          let time = Hashtbl.find result.times rid in
          let cumul = Hashtbl.find result.cumulated_wait rid in
          printf "%s %s (%s: %i; %i; start @ %i)\n" (String.make 0 '-')
            (Research.to_string rid)
            (Research.human_name_exn rid)
            time cumul when_ )
        by_start_time ;
      printf "total_time;used_labs;total_cost %s\n"
        (Lab.result_to_string result)

let print_similar_research s =
  let ls = String.lowercase_ascii s in
  Research.(
    iter_research (fun rid ->
        let name = String.lowercase_ascii (human_name_exn rid) in
        let id = String.lowercase_ascii (Research.to_string rid) in
        if
          Core.String.is_substring ~substring:ls name
          || Core.String.is_substring ~substring:ls id
        then
          let components = Research.enabled_components rid in
          let structures = Research.enabled_structures rid in
          let comps = String.concat " " (components @ structures) in
          printf "(%s) %s enables: [%s]\n" (human_name_exn rid)
            (Research.to_string rid) comps ))

(* TODO let user choose what columns he wants *)
let run_result_to_string (r : run_result) =
  let sprintf = Printf.sprintf in
  sprintf "%i;%i;%i;%i" r.long_min_upgrades.salvos r.short_min_upgrades.salvos
    r.long_min_upgrades.sec r.short_min_upgrades.sec

let run_result_headers () =
  String.concat ";"
    [ "long_min_upgrades_salvo"
      (* ; "long_all_upgrades_salvo" *)
      (* ; "long_all_upgrades_sec" *)
    ; "short_min_upgrades_salvo"
    ; "long_min_upgrades_sec"
    ; "short_min_upgrades_sec"
      (* ; "short_all_upgrades_salvo" *)
      (* ; "short_all_upgrades_sec" *) ]

let expected_salvos_to_reach ~(agg_w : Weapon.t) ~(agg_ctx : Research.context)
    ~(target : Droid.t) ~target_ctx ~dist =
  let expected_dmg, expected_periodical_dmg =
    let dmg = Droid.calc_dmg target agg_w ~agg_ctx ~target_ctx in
    let p_dmg = Droid.calc_dmg_periodical target agg_w ~agg_ctx ~target_ctx in
    let hitchance = Weapon.effective_hitchance_at_dist agg_w agg_ctx dist in
    let out = Pct.scale_by hitchance dmg in
    (max out 1, Pct.scale_by hitchance p_dmg)
  in
  let target_hp = Droid_template.effective_hp target.template target_ctx in
  let dmg_per_salvo = expected_dmg * agg_w.numRounds in
  let effective_reload_time = Weapon.effective_reload_time agg_w agg_ctx in
  let effective_firepause = Weapon.effective_firepause agg_w agg_ctx in
  let _ = agg_w.penetrate in
  (* in case weapon doesn't have any long lasting effects *)
  let no_periodical () =
    (* truncated number of salvos needed *)
    let salvos_needed = target_hp / dmg_per_salvo in
    let rest = target_hp - (dmg_per_salvo * salvos_needed) in
    let shots_needed = Utils.idiv rest expected_dmg in
    (* between each salvo, wait for reloadTime *)
    let time_for_salvos = (effective_reload_time - 1) * salvos_needed in
    Dolog.Log.debug
      "expected_dmg;dmg_per_salvo;salvos_needed: %i;%i;%i;%i;%i %i" expected_dmg
      dmg_per_salvo salvos_needed shots_needed effective_firepause target_hp ;
    (* between each shot, wait for firePause *)
    let time_for_shots = (effective_firepause - 1) * shots_needed in
    Weapon_types.expected_of
      (Utils.idiv target_hp dmg_per_salvo)
      (* firePause 10 = 1.0 seconds *)
      ((time_for_salvos + time_for_shots) / 10)
  in
  (* otherwise, weapon also has periodical damage:
     periodical damage is by definition, damage per 1 second.
      However, it's almost impossible to lit enemy on fire with Flamers because
      the projectile penetrates, and keeps flying further,
      which means the tile below the target is *not* on fire;
      which means *no* periodical damage is applied.
      On the other hand, Bombs do not penetrate, and explode immediately on
      the target, thus triggering periodical damage in 100% cases.
  *)
  let with_periodical () =
    let fl_effective_firepause = Float.of_int effective_firepause in
    let expected_dmg_to_deal =
      if agg_w.penetrate then ref (Float.of_int expected_periodical_dmg)
      else ref 0.0
    in
    let hp_left = ref (Float.of_int target_hp) in
    let cur_time = ref 0 in
    let salvos = ref 0. in
    (* TODO: implement with reload *)
    if effective_reload_time = effective_firepause then (
      Dolog.Log.error
        "simulations for weapons with reload time (%s) are not implemented yet"
        agg_w.name ;
      exit 1 )
    else
      while !hp_left > 0. do
        (* move forward by 1 second *)
        cur_time := !cur_time + 10 ;
        (* how many shots we have made during last second? *)
        let nb_shots_possible = 10. /. fl_effective_firepause in
        salvos := nb_shots_possible +. !salvos ;
        expected_dmg_to_deal :=
          (nb_shots_possible *. 4.) +. !expected_dmg_to_deal ;
        hp_left := !hp_left -. !expected_dmg_to_deal
      done ;
    Weapon_types.expected_of (Float.to_int !salvos) (!cur_time / 10)
  in
  if expected_periodical_dmg = 0 then no_periodical () else with_periodical ()

let run_weapon_vs_template (agg_w : Weapon.t)
    (target_template : Droid_template.t) =
  let target_ctx = Research.empty () in
  let agg_ctx = Research.empty () in
  let target = Droid.of_template target_template target_ctx in
  if not (Droid.is_susceptible_to target agg_w) then None
  else (
    Research.(grant_upto_research (component_enabled_exn agg_w.id) agg_ctx) ;
    Droid_template.grant_all_for target_template target_ctx ;
    let long_min_upgrades =
      expected_salvos_to_reach ~agg_w ~agg_ctx ~target ~target_ctx
        ~dist:agg_w.longRange
    in
    let short_min_upgrades =
      expected_salvos_to_reach ~agg_w ~agg_ctx ~target ~target_ctx
        ~dist:agg_w.shortRange
    in
    Some {long_min_upgrades; short_min_upgrades} )

(* Research.grant_all_upgrades target_ctx;
   Research.grant_all_upgrades agg_ctx;
   let long_all_upgrades = expected_salvos_to_overtake agg_w agg_ctx ~target ~target_ctx agg_w.longRange in
   let short_all_upgrades = expected_salvos_to_overtake agg_w agg_ctx ~target ~target_ctx agg_w.shortRange in *)

(** sort & print weapons by expected dmg, among all weapons or only those of some subclass *)
(* let sort_weapons subclass =
     let subclass_cmpr scl acc (w : Weapon.t) =
       match w.dmgSubClass with
       | x when x = scl -> w :: acc
       | _ -> acc
     in
     let comparator s =
       try subclass_cmpr (Weapon.weapon_subclass_of_string s) with
       | Invalid_argument _ -> fun acc (w : Weapon.t) -> w :: acc
     in
     let (filtered : Weapon.t list) = Weapon.fold_weapons (comparator subclass) [] in
     let sorted =
       List.sort
         (fun (a : Weapon.t) (b : Weapon.t) -> Weapon.(normalized_expected_dmg_short a - normalized_expected_dmg_short b))
         filtered
     in
     List.iter (fun (x : Weapon.t) -> (printf "%s %i" x.name x.damage)) sorted
   ;; *)
