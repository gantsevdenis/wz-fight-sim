let confdir =
  let open Filename in
  Printf.sprintf "%s"
    (String.concat ""
       [parent_dir_name; dir_sep; "lib"; dir_sep; "data-4.2.7"; dir_sep] )

let set_dbg_level s =
  let open Dolog.Log in
  match s with
  | "info" ->
      set_log_level INFO
  | "debug" ->
      set_log_level DEBUG
  | "warn" ->
      set_log_level WARN
  | "error" ->
      set_log_level ERROR
  | s ->
      raise (Invalid_argument ("unknown debug level:" ^ s))

type subcmd_template_t = Template_info of string

type subcmd_research_t =
  | Research_Find_id of string
  | Research_Bulk_time_to_res of string
  | Research_Tree of string
  | Research_Undef

type subcmd_fight_t =
  | Fight_weapon_vs_template of string
  | Fight_weapon_vs_1 of string * string

type cmd_conf =
  | Cmd_Research of subcmd_research_t
  | Cmd_Fight of subcmd_fight_t
  | Cmd_Template of subcmd_template_t
  | Cmd_Initialized (* debug, --conf-dir passed *)
  | Cmd_Undef

let print_err_and_exit s = Dolog.Log.error "%s" s ; exit 1

let parse_sub_template () =
  if Array.length Sys.argv < 4 then (
    Dolog.Log.error "too few arguments" ;
    exit 1 ) ;
  match Sys.argv.(2) with
  | "info" ->
      Template_info Sys.argv.(3)
  | s ->
      print_err_and_exit s

let parse_sub_research () =
  if Array.length Sys.argv < 4 then (
    Dolog.Log.error "too few arguments" ;
    exit 1 ) ;
  match Sys.argv.(2) with
  | "tree" ->
      Research_Tree Sys.argv.(3)
  | "bulk-time-to-res" ->
      Research_Bulk_time_to_res Sys.argv.(3)
  | "find" ->
      Research_Find_id Sys.argv.(3)
  | s ->
      print_err_and_exit s

let parse_sub_fight () =
  if Array.length Sys.argv < 4 then (
    Dolog.Log.error "too few arguments" ;
    exit 1 ) ;
  match Sys.argv.(2) with
  | "weapon-vs-template" ->
      Fight_weapon_vs_template Sys.argv.(3)
  | "1vs1" ->
      Fight_weapon_vs_1 (Sys.argv.(3), Sys.argv.(4))
  | s ->
      print_err_and_exit s

(* let parse_optionals () =
   match Sys.argv.(!cur_arg_idx) with
   | "--verbosity" ->
       cur_arg_idx := !cur_arg_idx + 1 ;
       set_dbg_level Sys.argv.(!cur_arg_idx) ;
       cur_arg_idx := !cur_arg_idx + 1
   | s ->
       print_err_and_exit s *)

(* let parse_mandatory () =
   match Sys.argv.(!cur_arg_idx) with
   | "--conf-dir" ->
       cur_arg_idx := !cur_arg_idx + 1 ;
       confdir := Sys.argv.(!cur_arg_idx) ;
       cur_arg_idx := !cur_arg_idx + 1
   | s ->
       print_err_and_exit s *)

let parse_args () =
  if Array.length Sys.argv = 1 then Cmd_Undef
  else
    match Sys.argv.(1) with
    | "research" ->
        Cmd_Research (parse_sub_research ())
    | "fight" ->
        Cmd_Fight (parse_sub_fight ())
    | "template" ->
        Cmd_Template (parse_sub_template ())
    | s ->
        print_err_and_exit s
