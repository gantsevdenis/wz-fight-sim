include Research_types
open Core
open Utils
module StrTable = Hashtbl.Make (String)
module IntTable = Hashtbl.Make (Int)

type t = int

let _states = Array.create ~len:406 Uninitilized

let _cost = Array.create ~len:406 0

let _cost_points = Array.create ~len:406 0

let _is_research_upgrade = IntTable.create ()

let _is_module_upgrade : t ref = ref (-1)

let _str_to_idx = StrTable.create ()

let _idx_to_str = IntTable.create ()

let _human_names = IntTable.create ()

let results = IntTable.create ()

let prerequisites = IntTable.create ()

let children = IntTable.create ()

let _components_enabled = StrTable.create ()

let _structures_enabled = StrTable.create ()

let _enabled_components = IntTable.create ()

let _enabled_structures = IntTable.create ()

let is_research_upgrade rid =
  match IntTable.find _is_research_upgrade rid with
  | None ->
      false
  | Some _ ->
      true

let cost rid = _cost.(rid)

let cost_points rid = _cost_points.(rid)

let component_enabled s = StrTable.find _components_enabled s

let structure_enabled s = StrTable.find _structures_enabled s

let component_enabled_exn s = StrTable.find_exn _components_enabled s

let structure_enabled_exn s = StrTable.find_exn _structures_enabled s

let enabled_components rid = IntTable.find _enabled_components rid |> or_else []

let enabled_structures rid = IntTable.find _enabled_structures rid |> or_else []

(* object id -> research_id *)
let human_name_exn rid = IntTable.find_exn _human_names rid

let of_string_exn s = StrTable.find_exn _str_to_idx s

let of_string s = StrTable.find _str_to_idx s

let to_string rid = IntTable.find_exn _idx_to_str rid

let is_module_upgrade (rid : t) = !_is_module_upgrade = rid

type context = {states: state Array.t; mutable upgrades_done: Upgrades.t list}

let empty () = {states= Array.copy _states; upgrades_done= []}

let iter_upgrades ctx f = List.iter ~f ctx.upgrades_done

let fold_upgrades ctx f acc = List.fold_left ~f ~init:acc ctx.upgrades_done

let get_parents (id : t) : t list option = IntTable.find prerequisites id

let get_children_exn (id : t) : t list = IntTable.find_exn children id

let get_children (id : t) : t list option = IntTable.find children id

let get_upgrades (id : t) : Upgrades.t list option = IntTable.find results id

let is_completed id ctx =
  match ctx.states.(id) with Completed -> true | _ -> false

let is_possible id ctx =
  match ctx.states.(id) with Possible _ -> true | _ -> false

(** all incoming edges have been researched *)
let can_be_marked_possible rid ctx =
  match get_parents rid with
  | Some parents ->
      List.fold ~f:(fun acc x -> acc && is_completed x ctx) ~init:true parents
  | None ->
      true

let needed rid ctx =
  match ctx.states.(rid) with Possible n -> n | _ -> assert false

(** save researched upgrades into their respective lists *)
let add_upgrade up ctx =
  (* Dolog.Log.debug "adding upgrade: %s" (Upgrades.to_string up); *)
  ctx.upgrades_done <- up :: ctx.upgrades_done

let add_upgrades ctx ups = List.iter ~f:(fun up -> add_upgrade up ctx) ups

let mark_possible rid ctx =
  Array.set ctx.states rid
    ( match ctx.states.(rid) with
    | Impossible n ->
        Possible n
    | Completed ->
        raise (Invalid_argument "unexpected state: completed")
    | Possible _ ->
        raise (Invalid_argument "unexpected state: already possible")
    | Uninitilized ->
        raise (Invalid_argument "unexpected state: uninitialized") )

let update_children rid ctx =
  List.iter
    ~f:(fun ch ->
      if can_be_marked_possible ch ctx then mark_possible ch ctx else () )
    (get_children rid |> or_else [])

let add_points rid ctx points =
  let new_state =
    match ctx.states.(rid) with
    | Possible n ->
        let x = max (n - points) 0 in
        if x = 0 then Completed else Possible x
    | Completed ->
        raise
          (Invalid_argument
             (Printf.sprintf "cannot add to completed research! %i" rid) )
    | Impossible _ ->
        raise
          (Invalid_argument
             (Printf.sprintf "cannot add to unavailable research! %i" rid) )
    | Uninitilized ->
        raise (Invalid_argument (Printf.sprintf "Unintializd research array"))
  in
  Array.set ctx.states rid new_state ;
  match new_state with
  | Completed -> (
      update_children rid ctx ;
      match get_upgrades rid with
      | Some upgrades ->
          add_upgrades ctx upgrades ; true
      | None ->
          true )
  | _ ->
      false

let hash = Hashtbl.hash

let compare = Int.compare

let sum_research_upgrades ctx =
  let open Upgrades in
  List.fold_left
    ~f:(fun acc x ->
      acc
      +
      match x with
      | Upgrade (CLS_BUILDING, P_RESEARCH_POINTS, None, _) ->
          1
      | _ ->
          0 )
    ~init:0 ctx.upgrades_done

let memo f =
  let h = IntTable.create () in
  fun x ->
    match IntTable.find h x with
    | None ->
        let y = f x in
        IntTable.set h ~key:x ~data:y ;
        y
    | Some v ->
        v

(* get all parent nodes starting from ridx
   https://github.com/Warzone2100/warzone2100/blob/master/data/mp/multiplay/script/functions/camTechEnabler.js
*)
let _calc_dependencies (ridx : t) =
  let out = ref [] in
  let q = Queue.create () in
  Queue.enqueue q ridx ;
  let seen = IntTable.create () in
  while Queue.length q > 0 do
    let item = Queue.dequeue_exn q in
    if not (IntTable.mem seen item) then (
      IntTable.add_exn seen ~key:item ~data:0 ;
      out := item :: !out ;
      let parents = get_parents item |> or_else [] in
      List.iter
        ~f:(fun x -> if not (IntTable.mem seen x) then Queue.enqueue q x)
        parents )
  done ;
  !out

let calc_dependencies = memo _calc_dependencies

let grant_all_upgrades ctx =
  Array.iteri
    ~f:(fun i _ ->
      let ups : Upgrades.t list = get_upgrades i |> or_else [] in
      add_upgrades ctx ups )
    ctx.states

let grant_research_exn rid ctx =
  if not (is_completed rid ctx) then (
    Array.set ctx.states rid
      ( match ctx.states.(rid) with
      | Completed ->
          raise
            (Invalid_argument
               (Printf.sprintf "already completed! %s" (to_string rid)) )
      | _ ->
          Completed ) ;
    add_upgrades ctx (get_upgrades rid |> or_else []) )

let grant_research rid ctx =
  if not (is_completed rid ctx) then (
    Array.set ctx.states rid (match ctx.states.(rid) with _ -> Completed) ;
    add_upgrades ctx (get_upgrades rid |> or_else []) )

let grant_upto_research rid ctx =
  List.iter ~f:(fun d -> grant_research d ctx) (calc_dependencies rid)

let grant_upto_research_opt rid_opt ctx =
  match rid_opt with None -> () | Some rid -> grant_upto_research rid ctx

let iter_research f =
  for i = 0 to Array.length _states - 1 do
    f i
  done

let fold_research ~f ~(init : 'a) =
  let cur = ref init in
  for i = 0 to Array.length _states - 1 do
    cur := f !cur i
  done ;
  !cur

let effective_research_points ctx ~(has_module : bool) =
  14 + (sum_research_upgrades ctx * 5) + if has_module then 7 else 0

let contains (x : t) (xl : t list) = List.mem ~equal:Int.equal xl x

let equal a b = Int.equal a b

let conv_upgrade_class s =
  match s with
  | "Body" ->
      "CLS_BODY"
  | "Brain" ->
      "CLS_BRAIN"
  | "Building" ->
      "CLS_BUILDING"
  | "Construct" ->
      "CLS_CONSTRUCT"
  | "Sensor" ->
      "CLS_SENSOR"
  | "Weapon" ->
      "CLS_WEAPON"
  | _ ->
      raise (Invalid_argument s)

let conv_filter_param s =
  match s with
  | "BodyClass" ->
      "FP_BODYCLASS"
  | "Id" ->
      "FP_ID"
  | "ImpactClass" ->
      "FP_IMPACTCLASS"
  | "Type" ->
      "FP_TYPE"
  | _ ->
      raise (Invalid_argument s)

let conv_filter_value s =
  match s with
  | "A-A GUN" ->
      "FV_AA_GUN"
  | "BOMB" ->
      "FV_BOMB"
  | "CANNON" ->
      "FV_CANNON"
  | "CommandBrain01" ->
      "FV_COMMANDBRAIN01"
  | "Cyborgs" ->
      "FV_CYBORGS"
  | "Droids" ->
      "FV_DROIDS"
  | "ENERGY" ->
      "FV_ENERGY"
  | "FLAME" ->
      "FV_FLAME"
  | "GAUSS" ->
      "FV_GAUSS"
  | "HOWITZERS" ->
      "FV_HOWITZERS"
  | "MACHINE GUN" ->
      "FV_MACHINE"
  | "MISSILE" ->
      "FV_MISSILE"
  | "MORTARS" ->
      "FV_MORTARS"
  | "ROCKET" ->
      "FV_ROCKET"
  | "Structure" ->
      "FV_STRUCTURE"
  | "Wall" ->
      "FV_WALL"
  | _ ->
      raise (Invalid_argument s)

let conv_upgrade_param s =
  match s with
  | "Armour" ->
      "P_ARMOUR"
  | "BaseCommandLimit" ->
      "P_BASE_COMMAND_LIMIT"
  | "CommandLimitByLevel" ->
      "P_COMMAND_LIMIT_BY_LEVEL"
  | "ConstructorPoints" ->
      "P_CONSTRUCTOR_POINTS"
  | "Damage" ->
      "P_DAMAGE"
  | "FirePause" ->
      "P_FIRE_PAUSE"
  | "HitChance" ->
      "P_HIT_CHANCE"
  | "HitPointPct" ->
      "P_HIT_POINT_PCT"
  | "HitPoints" ->
      "P_HIT_POINTS"
  | "Power" ->
      "P_POWER"
  | "PowerPoints" ->
      "P_POWER_POINTS"
  | "ProductionPoints" ->
      "P_PRODUCTION_POINTS"
  | "RadiusDamage" ->
      "P_RADIUS_DAMAGE"
  | "Range" ->
      "P_RANGE"
  | "RankThresholds" ->
      "P_RANK_THRESHOLDS"
  | "RearmPoints" ->
      "P_REARM_POINTS"
  | "ReloadTime" ->
      "P_RELOAD_TIME"
  | "RepairPoints" ->
      "P_REPAIR_POINTS"
  | "RepeatDamage" ->
      "P_REPEAT_DAMAGE"
  | "ResearchPoints" ->
      "P_RESEARCH_POINTS"
  | "Resistance" ->
      "P_RESISTANCE"
  | "ShortHitChance" ->
      "P_SHORT_HIT_CHANCE"
  | "Thermal" ->
      "P_THERMAL"
  | _ ->
      raise (Invalid_argument s)

let _handle_upgrades data : Upgrades.t list =
  let results =
    match U.member "results" data with `Null -> [] | s -> U.to_list s
  in
  let open Upgrades in
  List.fold_left
    ~f:(fun acc res ->
      let clazz =
        upgrade_class_of_string (conv_upgrade_class (get_string "class" res))
      in
      let param =
        upgrade_parameter_of_string
          (conv_upgrade_param (get_string "parameter" res))
      in
      let value : int = 100 + (U.member "value" res |> U.to_int) in
      let fparam_js = U.member "filterParameter" res |> U.to_string_option in
      let fvalue_js = U.member "filterValue" res |> U.to_string_option in
      let upgrade =
        match (fparam_js, fvalue_js) with
        | None, None ->
            Upgrade (clazz, param, None, Pct.of_int value)
        | Some p, Some v ->
            let fparam, fvalue =
              ( filter_param_of_string (conv_filter_param p)
              , filter_value_of_string (conv_filter_value v) )
            in
            Upgrade (clazz, param, Some (fparam, fvalue), Pct.of_int value)
        | _ ->
            assert false
      in
      upgrade :: acc )
    ~init:[] results

(** returns list of prerequisite indices (as strings) *)
let add_prerequisites_children i data =
  let rrtext : string list = get_string_list "requiredResearch" data in
  let rr_idxs = List.fold_left ~f:(fun acc r -> r :: acc) ~init:[] rrtext in
  List.iter
    ~f:(fun parent ->
      IntTable.add_multi children ~key:(of_string_exn parent) ~data:i )
    rr_idxs ;
  List.iter
    ~f:(fun parent ->
      IntTable.add_multi prerequisites ~key:i ~data:(of_string_exn parent) )
    rr_idxs ;
  List.length rr_idxs

let add_resultComponents i v =
  let components = get_string_list "resultComponents" v in
  List.iter
    ~f:(fun comp ->
      StrTable.add_exn _components_enabled ~key:comp ~data:i ;
      IntTable.add_multi _enabled_components ~key:i ~data:comp )
    components

let add_resultStructures i v =
  let structures = get_string_list "resultStructures" v in
  List.iter
    ~f:(fun s ->
      StrTable.add_exn _structures_enabled ~key:s ~data:i ;
      IntTable.add_multi _enabled_structures ~key:i ~data:s )
    structures

let init () =
  let data = Yojson.Safe.from_file (sprintf "%sresearch.json" Conf.confdir) in
  let data_assoc = U.to_assoc data in
  let ordered =
    List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) data_assoc
  in
  List.iteri
    ~f:(fun i (k, _) ->
      StrTable.set _str_to_idx ~key:k ~data:i ;
      IntTable.set _idx_to_str ~key:i ~data:k )
    ordered ;
  let nb_res = ref 0 in
  List.iteri
    ~f:(fun i (k, v) ->
      nb_res := !nb_res + 1 ;
      if String.equal "R-Struc-Research-Module" k then _is_module_upgrade := i ;
      if String.is_substring ~substring:"R-Struc-Research-Upgrade" k then
        IntTable.add_exn _is_research_upgrade ~key:i ~data:0 ;
      let how_many = add_prerequisites_children i v in
      add_resultComponents i v ;
      add_resultStructures i v ;
      let rpoints = get_int "researchPoints" v in
      let rpower = get_int "researchPower" v in
      let state_item =
        if how_many = 0 then Possible rpoints else Impossible rpoints
      in
      let name = get_string "name" v in
      Dolog.Log.debug "research item: %s %s %i" k name i ;
      IntTable.add_exn _human_names ~key:i ~data:name ;
      Array.set _states i state_item ;
      Array.set _cost i rpower ;
      Array.set _cost_points i rpoints ;
      List.iter
        ~f:(fun up -> IntTable.add_multi results ~key:i ~data:up)
        (_handle_upgrades v) )
    ordered ;
  assert (!nb_res = 406)
