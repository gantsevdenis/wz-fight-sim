let production_rate ?(modules = 0) (rctx : Research.context) =
  assert (modules <= 2);
  let all_ups =
    Research.fold_upgrades
      rctx
      (fun acc up ->
        let open Upgrades in
        match up with
        | Upgrade (CLS_BUILDING, P_PRODUCTION_POINTS, None, pct) -> Pct.(pct + acc)
        | _ -> acc)
      Pct.cent
  in
  (* Dolog.Log.debug "upgrades: %i" (Pct.scale_by all_ups 100); *)
  (modules * 10) + Pct.scale_by all_ups 10
;;

(* let total_build_time (templ : Droid_template.t) p_r = Droid_template.total_build_points templ / p_r *)
