(** This module provides result type and functions for compatibility
 * with OCaml 4.06 *)

type error =
  [ `Premature_end_of_input
  | `Unknown_field_type of int
  | `Wrong_field_type of string * Field.t
  | `Illegal_value of string * Field.t
  | `Not_implemented
  | `Unknown_enum_value of int
  | `Oneof_missing
  | `Required_field_missing ]

let show_error = function
  | `Premature_end_of_input -> "`Premature_end_of_input"
  | `Unknown_field_type i -> Printf.sprintf "`Unknown_field_type: %d" i
  | `Wrong_field_type (s, f) -> Printf.sprintf "`Wrong_field_type: %s - %s" s (Field.show f)
  | `Illegal_value (s, f) -> Printf.sprintf "`Illegal_value: %s - %s" s (Field.show f)
  | `Not_implemented-> "`Not_implemente"
  | `Unknown_enum_value i -> Printf.sprintf "`Unknown_enum_value: %d" i
  | `Oneof_missing-> "`Oneof_missing"
  | `Required_field_missing -> "`Required_field_missing"

type 'a t = ('a, error) result

let ( >>| ) v f = match v with Ok x -> Ok (f x) | Error err -> Error err
let ( >>= ) v f = match v with Ok x -> f x | Error err -> Error err

(* Extra functions (from Base) *)

let return x = Ok x
let fail x = Error x
