open Core

type state =
  | Impossible of int
  | Possible of int
  | Completed
  | Uninitilized

module StrTable = Hashtbl.Make (String)


