open! Fight_sim
open Dolog
open Core
open Utils

let () =
  Log.set_log_level Log.DEBUG ;
  Log.set_output stdout ;
  Log.color_on () ;
  Body.init () ;
  Structure.init () ;
  Weapon.init () ;
  Propulsion.init () ;
  Sensor.init () ;
  Research.init ()

let cannon1mk1 = Weapon.by_name_exn "Light Cannon"

let bastion = Structure.by_name_exn "Heavy Rocket Bastion"

let mg1mk1 = Weapon.by_name_exn "Machinegun"

let hvy_mg = Weapon.by_name_exn "Heavy Machinegun"

let flamer = Weapon.by_name_exn "Flamer"

let cyborgRocket = Weapon.by_name_exn "Cyborg Lancer"

let wheeled01 = Propulsion.by_name_exn "Wheels"

let tracked01 = Propulsion.by_name_exn "Tracks"

let half_tracks = Propulsion.by_name_exn "Half-tracks"

let cyborgLegs = Propulsion.by_id_exn "CyborgLegs"

let viper = Body.by_name_exn "Viper"

let cyborgLightBody = Body.by_id_exn "CyborgLightBody"

let machinegun_viper_wheels =
  Droid_template.of_components wheeled01 mg1mk1 viper

let machinegun_viper_halftracks =
  Droid_template.of_components half_tracks mg1mk1 viper

let light_cannon_viper_wheels =
  Droid_template.of_components wheeled01 cannon1mk1 viper

let lancer_cyborg =
  Droid_template.of_components cyborgLegs cyborgRocket cyborgLightBody

let flamer_viper_half_tracks =
  Droid_template.of_components half_tracks flamer viper

let flamer_viper_wheels = Droid_template.of_components wheeled01 flamer viper

let hvymg_viper_wheels = Droid_template.of_components wheeled01 hvy_mg viper

let check = Alcotest.check

module Common = struct
  let int = Alcotest.int

  (* let check_normalized_expected_long_dmg expected w = check int "" expected (Weapon.expected_dmg_long w) *)
  (* let check_normalized_expected_short_dmg expected w = check int "" expected (Weapon.expected_dmg_short w) *)
  let check_total_cost expected templ =
    check int "" expected (Droid_template.total_cost templ)

  let check_template_dmg expected w templ rctx =
    check int "" expected (Droid_template.calc_dmg templ w rctx)

  let check_droid_dmg expected w d rctx =
    check int "" expected (Droid.calc_dmg d w ~agg_ctx:rctx ~target_ctx:rctx)

  let check_template_weight expected templ =
    check int "" expected (Droid_template.total_weight templ)

  let check_template_hp expected templ rctx =
    check int "" expected (Droid_template.effective_hp templ rctx)
end

module TestDroid = struct
  open Common

  let rctx = Research.empty ()

  let machinegun_viper_wheels_droid =
    Droid.of_template machinegun_viper_wheels rctx

  let lightcannon_viper_wheels_droid =
    Droid.of_template light_cannon_viper_wheels rctx

  let lancer_cyborg_droid = Droid.of_template lancer_cyborg rctx

  let hvy_mg_viper_wheels_droid = Droid.of_template hvymg_viper_wheels rctx

  let hvy_mg_viper_ht_droid = Droid.of_template machinegun_viper_halftracks rctx

  let test_droid1 () = check_total_cost 55 machinegun_viper_wheels

  let test_droid2 () = check_template_hp 205 machinegun_viper_wheels rctx

  let test_droid3 () = check_template_dmg 10 mg1mk1 machinegun_viper_wheels rctx

  let test_droid4 () =
    check_droid_dmg 3 mg1mk1 machinegun_viper_wheels_droid rctx

  let test_droid21 () = check_total_cost 120 light_cannon_viper_wheels

  let test_droid22 () = check_template_hp 380 light_cannon_viper_wheels rctx

  (* dmg = 35; modifier = 125, result = 35 * 125 / 100 = 43 *)
  let test_droid23 () =
    check_template_dmg 43 cannon1mk1 light_cannon_viper_wheels rctx

  (* dmg = 35; modifier = 65, result = 35 * 65 / 100 = 22 *)
  let test_droid27 () = check_template_dmg 22 cannon1mk1 lancer_cyborg rctx

  (* dmg = 35; modifier = 125, result = (35 * 125 / 100)  - 10 = 33 *)
  let test_droid24 () =
    check_droid_dmg 33 cannon1mk1 machinegun_viper_wheels_droid rctx

  (* let test_25 () =
       check_normalized_expected_long_dmg 19 cannon1mk1;
       check_normalized_expected_short_dmg 24 cannon1mk1
     ;;

     let test_26 () =
       check_normalized_expected_long_dmg 5 mg1mk1;
       check_normalized_expected_short_dmg 7 mg1mk1
     ;; *)

  let test_droid25 () =
    check_droid_dmg 33 cannon1mk1 machinegun_viper_wheels_droid rctx ;
    check_droid_dmg 33 cannon1mk1 lightcannon_viper_wheels_droid rctx ;
    check_droid_dmg 10 cannon1mk1 lancer_cyborg_droid rctx ;
    check_droid_dmg 22 hvy_mg hvy_mg_viper_wheels_droid rctx ;
    check_droid_dmg 6 hvy_mg hvy_mg_viper_ht_droid rctx ;
    check_droid_dmg 26 flamer hvy_mg_viper_wheels_droid rctx

  let test_droid26 () = check_template_weight 3400 light_cannon_viper_wheels
end

let suite1 =
  let open TestDroid in
  let open Alcotest in
  [ ( "total_cost"
    , [ test_case "machinegun" `Quick test_droid1
      ; test_case "light cannon" `Quick test_droid21 ] )
  ; ( "total_hitpoints"
    , [ test_case "machinegun" `Quick test_droid2
      ; test_case "light cannon" `Quick test_droid22 ] )
  ; ( "template_damage"
    , [ test_case "machinegun" `Quick test_droid3
      ; test_case "light cannon" `Quick test_droid23
      ; test_case "light cannon" `Quick test_droid27 ] )
    (* ; "normalized_expected_dmg", [
       test_case "light cannon" `Quick test_25;
        test_case "machinegun" `Quick test_26 ] *)
  ; ("template weight", [test_case "light cannon" `Quick test_droid26]) ]

module TestResearch = struct
  let test1 () =
    let open Research in
    let rctx = empty () in
    let damage01, engineering =
      Research.(of_string_exn "R-Wpn-MG-Damage01", of_string_exn "R-Wpn-MG1Mk1")
    in
    let deps = calc_dependencies damage01 in
    check Alcotest.bool "" true (contains engineering deps) ;
    check Alcotest.bool "" false (is_completed damage01 rctx) ;
    check Alcotest.unit "" () (grant_upto_research damage01 rctx) ;
    check Alcotest.bool "" true (is_completed damage01 rctx) ;
    check Alcotest.bool "" true (is_completed engineering rctx)

  let test2 () =
    let open Research in
    let rctx = empty () in
    let vtol_factory, engineering =
      Research.
        (of_string_exn "R-Struc-VTOLFactory", of_string_exn "R-Wpn-MG1Mk1")
    in
    let deps = calc_dependencies vtol_factory in
    check Alcotest.bool "" false (is_completed engineering rctx) ;
    check Alcotest.bool "" false (is_completed vtol_factory rctx) ;
    check Alcotest.unit "" () (grant_upto_research vtol_factory rctx) ;
    check Alcotest.bool "" true (is_completed vtol_factory rctx) ;
    List.iter
      ~f:(fun d -> check Alcotest.bool "" true (is_completed d rctx))
      deps

  let test3 () =
    let count = ref 0 in
    let open Research in
    let open Common in
    let rctx = empty () in
    let damage01 = Research.of_string_exn "R-Wpn-MG-Damage01" in
    check_template_dmg 10 mg1mk1 machinegun_viper_wheels rctx ;
    check Alcotest.unit "" () (grant_upto_research damage01 rctx) ;
    iter_upgrades rctx (fun _ -> count := !count + 1) ;
    check Alcotest.bool "" true (!count = 1) ;
    Log.debug "nb upgrades %i" !count ;
    check Alcotest.bool "" false
      Pct.(cent = Weapon.sum_upgrades_dmg mg1mk1 rctx) ;
    check Alcotest.bool "" false
      Pct.(of_int 25 = Weapon.sum_upgrades_dmg mg1mk1 rctx) ;
    check int "" 13 (Pct.scale_by_iceil (Pct.of_int 125) 10) ;
    check_droid_dmg 4 mg1mk1
      (Droid.of_template machinegun_viper_wheels rctx)
      rctx ;
    check_droid_dmg 7 mg1mk1 (Droid.of_template lancer_cyborg rctx) rctx

  let test4 () =
    (* just check some research is generated correctly*)
    check Alcotest.int "" 2
      Research.(
        List.length
          ( get_upgrades (Research.of_string_exn "R-Sys-Resistance-Circuits")
          |> or_else [] )) ;
    check Alcotest.int "" 2
      Research.(
        List.length
          ( get_upgrades (Research.of_string_exn "R-Wpn-AAGun-ROF06")
          |> or_else [] ))

  let test41 () =
    let upgrade04 = Research.of_string_exn "R-Struc-Factory-Upgrade04" in
    let deps = Research.calc_dependencies upgrade04 in
    let contains =
      List.fold_left
        ~f:(fun acc x -> acc || Research.compare x upgrade04 = 0)
        ~init:false deps
    in
    (* target research is member of returned list *)
    check Alcotest.bool "" true contains

  let test5 () =
    let rctx = Research.empty () in
    check Alcotest.int "" 275
      (Droid_template.total_build_points machinegun_viper_wheels) ;
    check Alcotest.int "" 462
      (Droid_template.total_build_points flamer_viper_half_tracks) ;
    check Alcotest.int "" 10 (Manufacture.production_rate rctx) ;
    check Alcotest.int "" 20 (Manufacture.production_rate ~modules:1 rctx) ;
    check Alcotest.int "" 30 (Manufacture.production_rate ~modules:2 rctx) ;
    Research.(
      grant_upto_research
        (Research.of_string_exn "R-Struc-Factory-Upgrade01")
        rctx) ;
    check Alcotest.int "" 13 (Manufacture.production_rate rctx) ;
    check Alcotest.int "" 23 (Manufacture.production_rate ~modules:1 rctx) ;
    check Alcotest.int "" 33 (Manufacture.production_rate ~modules:2 rctx) ;
    (* note: there no upgrade02, neither upgrade03 *)
    let upgrade04 = Research.of_string_exn "R-Struc-Factory-Upgrade04" in
    check Alcotest.int "" 1
      Research.(List.length (get_upgrades upgrade04 |> or_else [])) ;
    check Alcotest.int "" 4
      (Research.fold_upgrades rctx (fun acc _ -> acc + 1) 0) ;
    Research.(grant_upto_research upgrade04 rctx) ;
    Research.iter_upgrades rctx (fun u ->
        Dolog.Log.debug "%s" (Upgrades.to_string u) ) ;
    (* 2 * 10 + (10 + 3 + 9)*)
    check Alcotest.int "" 42 (Manufacture.production_rate ~modules:2 rctx)

  let test6 () =
    check Alcotest.bool "Body Body1REC" true
      (Body.by_id "Body1REC" |> Option.is_some) ;
    check Alcotest.bool "Body Viper" true
      (Body.by_name "Viper" |> Option.is_some) ;
    check Alcotest.bool "Weapon Body1REC" true
      (Weapon.by_id "Body1REC" |> Option.is_none) ;
    check Alcotest.bool "Propulsion wheeled01" true
      (Propulsion.by_id "wheeled01" |> Option.is_some) ;
    check Alcotest.bool "Structure Hurricane AA Site" true
      (Structure.by_name "Hurricane AA Site" |> Option.is_some)

  let test7 () =
    (* must successfully calculate everythin*)
    Research.(
      iter_research (fun rid ->
          ignore (calc_dependencies rid) ;
          ignore
            ( try Lab.simulate_for_one rid
              with Not_found_s s ->
                Log.error "failed '%s' '%s' " (human_name_exn rid)
                  (to_string rid) ;
                raise (Not_found_s s) ) ))

  let test8 () =
    (* let ctx = Research.empty () in *)
    let eng = Research.of_string_exn "R-Sys-Engineering01" in
    let pillbox = Research.of_string_exn "R-Defense-Pillbox05" in
    check Alcotest.int "" 86 (Lab.simulate_for_one eng).total_wait ;
    Lab.max_labs := 1 ;
    check Alcotest.int "" 280 (Lab.simulate_for_one pillbox).total_wait ;
    Lab.max_labs := 2 ;
    check Alcotest.int "" 237 (Lab.simulate_for_one pillbox).total_wait

  let test9 () =
    check Alcotest.bool "" true
      (Option.is_some
         (Droid_template.of_string "light cannon viper half-tracks") ) ;
    check Alcotest.bool "" true
      (Option.is_some
         (Droid_template.of_string "half-tracks  light    cannon viper ") )

  let test10 () =
    let full = Research.empty () in
    let ctx = Research.empty () in
    Research.grant_all_upgrades full ;
    let templ = Droid_template.of_string_exn "Medium cannon python tracks" in
    Droid_template.grant_all_for templ ctx ;
    check Alcotest.int "" 75 (Weapon.effective_dmg templ.weapon ctx) ;
    check Alcotest.int "" 9 (Droid_template.effective_armour_heat templ ctx) ;
    check Alcotest.int "" 195 (Weapon.effective_dmg templ.weapon full) ;
    check Alcotest.int "" 4255 (Droid_template.effective_hp templ full) ;
    check Alcotest.int "" 37 (Weapon.effective_rof templ.weapon full) ;
    check Alcotest.int "" 74
      (Droid_template.effective_armour_kinetic templ full)
end

let suite2 =
  let open TestResearch in
  let open Alcotest in
  [ ( "grant_upto"
    , [ test_case "R-Wpn-MG-Damage01" `Quick test1
      ; test_case "R-Struc-VTOLFactory" `Quick test2
      ; test_case "machinegun upgrade" `Quick test3
      ; test_case "research gen" `Quick test4
      ; test_case "calc deps" `Quick test41
      ; test_case "build points" `Quick test5
      ; test_case "by_id" `Quick test6
      ; test_case "calc deps & simulate for all" `Quick test7
      ; test_case "research time with multiple labs" `Quick test8
      ; test_case "droid template of string" `Quick test9
      ; test_case "full upgrades" `Quick test10 ] ) ]

let () =
  let open Alcotest in
  run ~argv:[|""|] ~and_exit:false "Basic" suite1 ;
  run ~argv:[|""|] ~and_exit:false "Research" suite2
