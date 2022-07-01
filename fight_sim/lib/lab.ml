open Core

type rid_t = Research.t

let max_labs = ref 5

module ResearchSimulator = struct
  type lab_state_t = Not_Built | Built_Idle | Working of rid_t

  type lab_t = {state: lab_state_t; has_module: bool}

  type t = {mutable labs: lab_t Array.t}

  let empty_lab () = {state= Not_Built; has_module= false}

  let empty () = {labs= Array.create ~len:!max_labs (empty_lab ())}

  let reset_lab i ctx =
    let lab = Array.get ctx.labs i in
    Array.set ctx.labs i {lab with state= Built_Idle}

  let add_module ctx =
    let i, l =
      match Core.Array.findi ~f:(fun _ l -> not l.has_module) ctx.labs with
      | None ->
          raise (Invalid_argument "too many modules")
      | Some v ->
          v
    in
    Array.set ctx.labs i {l with has_module= true}

  let add_modules n ctx =
    for _ = 0 to n - 1 do
      add_module ctx
    done

  let next_idle_lab ctx =
    Core.Array.findi
      ~f:(fun _ lab -> match lab.state with Built_Idle -> true | _ -> false)
      ctx.labs

  let next_not_build_lab ctx =
    Core.Array.findi
      ~f:(fun _ lab -> match lab.state with Not_Built -> true | _ -> false)
      ctx.labs

  let add_lab (st : t) =
    match next_not_build_lab st with
    | None ->
        raise (Invalid_argument "too many labs")
    | Some (i, lab) ->
        Array.set st.labs i {lab with state= Built_Idle}

  let add_labs n st =
    for _ = 0 to n - 1 do
      add_lab st
    done

  let count_built_idle_labs st : int =
    Core.Array.count
      ~f:(fun lab -> match lab.state with Built_Idle -> true | _ -> false)
      st.labs

  let trigger_module (l : rid_t list) st =
    List.iter
      ~f:(fun rid ->
        if Research.is_module_upgrade rid then
          add_modules (count_built_idle_labs st) st )
      l

  let count_busy_labs st =
    Core.Array.count
      ~f:(fun lab -> match lab.state with Working _ -> true | _ -> false)
      st.labs

  let count_total_labs st =
    Core.Array.count
      ~f:(fun lab ->
        match lab.state with
        | Working _ ->
            true
        | Built_Idle ->
            true
        | _ ->
            false )
      st.labs

  (** Start research. This doesn't allow to stop/restart a research without loosing all points. *)
  let dispatch_research (rid : rid_t) ctx =
    match next_idle_lab ctx with
    | None ->
        raise (Invalid_argument "no lab available")
    | Some (idx, lab) ->
        let open Array in
        (* print_endline
           (Printf.sprintf
              "starting: %s %i"
              (ResearchCtx.idx_to_str rid ctx.rctx)
              (RIndex.to_int rid)); *)
        set ctx.labs idx {lab with state= Working rid}

  (** Iterate over currenly busy labs. Call "f" with index of each lab *)
  let iteri_wokring_labs f st =
    for i = 0 to !max_labs - 1 do
      let lab = Array.get st.labs i in
      match lab.state with Working rid -> f i lab rid | _ -> ()
    done

  let update_invariant (st : t) ctx =
    iteri_wokring_labs
      (fun i _ rid -> if Research.is_completed rid ctx then reset_lab i st)
      st

  (* add research points to all current research, for n seconds. Returns completed research ids *)
  let advance_by (n : int) (st : t) ctx =
    if n <= 0 then raise (Invalid_argument "must be positive")
    else
      let out = ref [] in
      iteri_wokring_labs
        (fun _ lab rid ->
          let new_points =
            Research.effective_research_points ~has_module:lab.has_module ctx
            * n
          in
          if Research.add_points rid ctx new_points then out := rid :: !out )
        st ;
      update_invariant st ctx ;
      !out

  let advance_by_ignore n st ctx = ignore (advance_by n st ctx : rid_t list)

  let is_being_researched rid ctx =
    match
      Array.find
        ~f:(fun l ->
          match l.state with
          | Working cur ->
              Research.equal rid cur
          | _ ->
              false )
        ctx.labs
    with
    | None ->
        false
    | Some _ ->
        true

  (** A research is technically available, and not already being researched ? *)
  let is_available_in_lab rid (st : t) ctx =
    Research.is_possible rid ctx && not (is_being_researched rid st)

  let research_rate lab ctx =
    Research.effective_research_points ctx ~has_module:lab.has_module

  let dump_lab_state st ctx =
    iteri_wokring_labs
      (fun i lab rid ->
        let need_pts = Research.needed rid ctx in
        let need_time = need_pts / research_rate lab ctx in
        Dolog.Log.error "lab;has_module;time %i;%b;%s;%i" i lab.has_module
          (Research.to_string rid) need_time )
      st

  let times_per_lab st ctx =
    let out = ref [] in
    iteri_wokring_labs
      (fun _ lab rid ->
        let need_pts = Research.needed rid ctx in
        let need_time = need_pts / research_rate lab ctx in
        let need_time =
          if need_pts mod research_rate lab ctx > 0 then need_time + 1
          else need_time
        in
        out := (rid, need_time) :: !out )
      st ;
    !out

  let how_long_left_all_seconds st ctx =
    assert (count_busy_labs st > 0) ;
    let out =
      List.fold_left (times_per_lab st ctx)
        ~f:(fun acc (_, t) -> max acc t)
        ~init:0
    in
    assert (out > 0) ;
    out

  let how_long_first_seconds st ctx =
    assert (count_busy_labs st > 0) ;
    let out =
      List.fold_left (times_per_lab st ctx)
        ~f:(fun acc (_, t) -> min acc t)
        ~init:Int.max_value
    in
    assert (out > 0) ;
    if out = Int.max_value then (
      dump_lab_state st ctx ;
      assert false ) ;
    out

  let how_long_till (principal : rid_t list) (secondary : rid_t list) (st : t)
      ctx =
    let total_wait = ref 0 in
    let total_cost = ref 0 in
    let max_busy_labs = ref 0 in
    let cumulated_wait = Stdlib.Hashtbl.create (List.length principal) in
    let when_started = ref [] in
    (* use Stdlib hashtable because Research.t is opaque *)
    let times = Stdlib.Hashtbl.create (List.length principal) in
    while
      (* tant que tous ce qui est à chercher, n'a pas été complété ... *)
      not
        (List.fold_left
           ~f:(fun acc x -> acc && Research.is_completed x ctx)
           ~init:true principal )
    do
      List.iter
        ~f:(fun rid ->
          if count_built_idle_labs st > 0 && is_available_in_lab rid st ctx then (
            dispatch_research rid st ;
            when_started := (rid, !total_wait) :: !when_started ) )
        principal ;
      let currently_busy = count_busy_labs st in
      max_busy_labs := max !max_busy_labs currently_busy ;
      assert (!max_busy_labs <= !max_labs) ;
      (* optionally, dispatch whatever we have to keep labs busy *)
      List.iter
        ~f:(fun rid ->
          if count_built_idle_labs st > 0 && is_available_in_lab rid st ctx then
            dispatch_research rid st )
        secondary ;
      List.iter
        ~f:(fun (rid, t) ->
          if Stdlib.Hashtbl.mem times rid then ()
          else Stdlib.Hashtbl.add times rid t )
        (times_per_lab st ctx) ;
      let to_wait = how_long_first_seconds st ctx in
      assert (to_wait > 0) ;
      total_wait := !total_wait + to_wait ;
      let completed = advance_by to_wait st ctx in
      assert (List.length completed > 0) ;
      List.iter
        ~f:(fun c ->
          total_cost := Research.cost c + !total_cost ;
          Stdlib.Hashtbl.add cumulated_wait c !total_wait )
        completed ;
      trigger_module completed st
    done ;
    ( !total_wait
    , !max_busy_labs
    , !total_cost
    , times
    , cumulated_wait
    , !when_started )
end

(** [total_wait]: total user's wait time until research is completed 
    [used_labs]: how many max labs were used instanteniously *)
type result =
  { total_wait: int
  ; used_labs: int
  ; total_cost: int
  ; times: (Research.t, int) Stdlib.Hashtbl.t
  ; cumulated_wait: (Research.t, int) Stdlib.Hashtbl.t
  ; when_started: (Research.t * int) list }

let result_to_string r =
  let mins = r.total_wait / 60 in
  let secs = r.total_wait mod 60 in
  (* FIXME: this is debug only *)
  (* Stdlib.Hashtbl.iter
     (fun k v -> printf "%s took %i\n" (Research.to_string k) v)
     r.times ; *)
  sprintf "%imin%isec;%i;%i" mins secs r.used_labs r.total_cost

(** [principal]: main research target
    [secondary]: optional secondary research target. Will be pursued whenever a lab is idle *)
let run_sim principal secondary sim ctx =
  let open ResearchSimulator in
  add_labs !max_labs sim ;
  assert (count_total_labs sim > 0) ;
  let to_wait, max_busy, total_cost, times, cumulated_wait, when_started =
    how_long_till principal secondary sim ctx
  in
  { total_wait= to_wait
  ; used_labs= max_busy
  ; total_cost
  ; times
  ; cumulated_wait
  ; when_started }

let simulate_for_all (rlist : Research.t list) =
  let rctx = Research.empty () in
  let sim = ResearchSimulator.empty () in
  run_sim rlist [] sim rctx

let simulate_for_one (rid : Research.t) =
  let rctx = Research.empty () in
  let sim = ResearchSimulator.empty () in
  let principal = Research.calc_dependencies rid in
  run_sim principal [] sim rctx
