(** This module provides result type and functions for compatibility
 * with OCaml 4.06 *)

type ('a, 'b) t = ('a, 'b) result

(* This modules provides for 4.06 compatibility *)
let map f = function Ok x -> Ok (f x) | Error err -> Error err

let bind f = function Ok x -> f x | Error err -> Error err

(* Extra functions (from Base) *)

let return x = Ok x

let ok_unit = Ok ()

let fail x = Error x
