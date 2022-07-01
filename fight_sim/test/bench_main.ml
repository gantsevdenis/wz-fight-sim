(* open! Wz_research
open! Wz_research.Droid_template_autogen
open Dolog
open Core
open Utils

let () =
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;
  Log.color_on ()
;;

let () = Weapon.init ()
let () = Propulsion.init ()
let () = Body.init ()
let () = Sensor.init ()
let () = Structure.init ()
let () = Research.init ()

module StandardTest = struct

let bench () = 

end

let () =

  let b1 = Bench.Test.create ~name:"Standard" MatchString.bench in
  let c = Bench.make_command [b1] in
  Command_unix.run c *)