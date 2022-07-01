open Fight_sim

let printf = Printf.printf

let run_weapon_vs_1_template w_spec t_spec =
  match Weapon.find_similar w_spec with
  | None ->
      Dolog.Log.error "%s is not recognized as a weapon" w_spec
  | Some agg_w -> (
    match Droid_template.of_string t_spec with
    | None ->
        Dolog.Log.error "%s is not recognized as a template" t_spec
    | Some target_template -> (
      match Analysis.run_weapon_vs_template agg_w target_template with
      | None ->
          ()
      | Some r ->
          printf "weapon;template;%s\n" (Analysis.run_result_headers ()) ;
          printf "%s;%s;%s\n" agg_w.name
            (Droid_template.generic_name target_template)
            (Analysis.run_result_to_string r) ) )

let weapons_vs_templates specfile =
  Dolog.Log.debug "loading spec from %s" specfile ;
  let open Core in
  let module StrTable = Hashtbl.Make (String) in
  let k_a_w, k_v_w, k_v_b, k_v_p =
    ( "[aggressor_weapon]"
    , "[victim_weapon]"
    , "[victim_body]"
    , "[victim_propulsion]" )
  in
  let keys = [k_a_w; k_v_w; k_v_b; k_v_p] in
  let spec = Utils.load_spec_file keys specfile in
  List.iter
    ~f:(fun k ->
      match StrTable.find spec k with
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "header %s in file %s contained no values: at least one \
                   expected"
                  specfile k ) )
      | Some _ ->
          () )
    keys ;
  printf "weapon;template;%s\n" (Analysis.run_result_headers ()) ;
  List.iter
    ~f:(fun spec_agg_w ->
      let agg_w = Weapon.by_name_exn spec_agg_w in
      List.iter
        ~f:(fun spec_vic_w ->
          let vic_w = Weapon.by_name_exn spec_vic_w in
          List.iter
            ~f:(fun spec_body ->
              match Body.by_name spec_body with
              | None ->
                  Dolog.Log.error "'%s' is not a valid body name" spec_body ;
                  Hashtbl.iter_keys Body.names_to_ids ~f:(fun name ->
                      Dolog.Log.error "%s" name ) ;
                  exit 1
              | Some body ->
                  List.iter
                    ~f:(fun spec_prop ->
                      let prop = Propulsion.by_name_exn spec_prop in
                      let target_template =
                        Droid_template.of_components prop vic_w body
                      in
                      match
                        Analysis.run_weapon_vs_template agg_w target_template
                      with
                      | None ->
                          ()
                      | Some r ->
                          printf "%s;%s;%s\n" agg_w.name
                            (Droid_template.generic_name target_template)
                            (Analysis.run_result_to_string r) )
                    (StrTable.find_exn spec k_v_p) )
            (StrTable.find_exn spec k_v_b) )
        (StrTable.find_exn spec k_v_w) )
    (StrTable.find_exn spec k_a_w)

let init_all () =
  Weapon.init () ;
  Propulsion.init () ;
  Body.init () ;
  Sensor.init () ;
  Structure.init () ;
  Research.init () ;
  Dolog.Log.set_output stderr ;
  Dolog.Log.color_on ()

let () =
  let open Core in
  match Conf.parse_args () with
  | Cmd_Undef ->
      Dolog.Log.error "expecting one of [fight] [research]" ;
      exit 1
  | Cmd_Fight (Fight_weapon_vs_template spec) ->
      init_all () ; weapons_vs_templates spec
  | Cmd_Fight (Fight_weapon_vs_1 (w, t1)) ->
      init_all () ;
      run_weapon_vs_1_template w t1
  | Cmd_Research rcmd -> (
    match rcmd with
    | Research_Find_id id ->
        Research.init () ;
        Analysis.print_similar_research id
    | Research_Tree id ->
        init_all () ;
        Analysis.print_dependencies id
    | _ ->
        Dolog.Log.error "unsupported." )
  | Cmd_Template (Template_info s) ->
      init_all () ; Analysis.template_info s
  | _ ->
      Dolog.Log.error "expecting one of [fight] [research]" ;
      exit 1
