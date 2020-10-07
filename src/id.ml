open Printf

type t = Id of string
[@@deriving show]

let make s = Id s

let print_string (Id s) = s

let dummy = make "_"

let c_name n = Utils.c_name (show n)

let count = ref 0

let get_temp _ =
  let c = !count in
  count := c + 1;
  let n = sprintf "_%d" c in
  make n
