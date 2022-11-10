open StdLabels

(** Taken from: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let is_reserved = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done"
  | "downto" | "else" | "end" | "exception" | "external" | "false" | "for" | "fun"
  | "function" | "functor" | "if" | "in" | "include" | "inherit" | "initializer"
  | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor" | "match" | "method"
  | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or"
  | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type"
  | "val" | "virtual" | "when" | "while" | "with" -> true
  | _ -> false

let to_snake_case ident =
  let to_list s =
    let r = ref [] in
    String.iter ~f:(fun c -> r := c :: !r) s;
    List.rev !r
  in
  let to_string l =
    let bytes = Bytes.create (List.length l) in
    List.iteri ~f:(fun i c -> Bytes.set bytes i c) l;
    Bytes.to_string bytes
  in
  let char_case = function
    | 'a' .. 'z' -> `Lower
    | 'A' .. 'Z' -> `upper
    | _ -> `Neither
  in
  let is_lower c = char_case c = `Lower in
  let is_upper c = char_case c = `Upper in

  let rec to_snake_case = function
    | c1 :: c2 :: cs when is_lower c1 && is_upper c2 ->
      c1 :: '_' :: c2 :: to_snake_case cs
    | c1 :: cs ->
      c1 :: (to_snake_case cs)
    | [] -> []
  in
  to_list ident
  |> to_snake_case
  |> to_string
  |> String.lowercase_ascii
  |> String.capitalize_ascii

let field_name ?(uncapitalize=true) ?(mangle_f=(fun x -> x)) field_name =
  let name = 
    let name =  mangle_f field_name in
    match uncapitalize with 
    | true -> String.uncapitalize_ascii name
    | false -> name 
  in
  match is_reserved name with
  | true -> name ^ "'"
  | false -> name 

let module_name ?(mangle_f=(fun x -> x)) name =
  let name = mangle_f name in
  match name.[0] with
  | '_' -> "P" ^ name
  | _ -> String.capitalize_ascii name

let poly_constructor_name ?(mangle_f=(fun x -> x)) name =
  "`" ^ (mangle_f name |> String.capitalize_ascii)

(** Local map with rpc name and ocaml variable name  *)
module Rpc = struct
  type t = (string, unit) Hashtbl.t
  let init () : t = Hashtbl.create 2
  let get_unique_name t preferred_name = 
    let rec inner name = 
      match Hashtbl.mem t name with
      | false -> name
      | true -> inner (name ^ "'")
   in 
   let name = inner preferred_name in
   Hashtbl.add t name ();
   name
end
