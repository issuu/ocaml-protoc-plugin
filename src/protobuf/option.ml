(** Functions for manipulating Option values for compatibility with 
 * OCaml 4.06 *)
let value ~default = function
  | None -> default
  | Some x -> x
let some x = Some x
