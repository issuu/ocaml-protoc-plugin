open StdLabels

(** Taken from: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let is_reserved = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done"
  | "downto" | "else" | "end" | "exception" | "external" | "false" | "for" | "fun"
  | "function" | "functor" | "if" | "in" | "include" | "inherit" | "initializer"
  | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor" | "match" | "method"
  | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or"
  | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type"
  | "val" | "virtual" | "when" | "while" | "with" ->
    true
  | _ -> false

let module_name name =
  match name.[0] with
  | '_' -> "P" ^ name ^ "'"
  | _ -> String.capitalize_ascii name

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
  let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c != c in
  let is_lower c = Char.lowercase_ascii c = c && Char.uppercase_ascii c != c in

  let rec to_snake_case = function
    | c1 :: c2 :: cs when is_lower c1 && is_upper c2 ->
      c1 :: '_' :: to_snake_case (c2 :: cs)
    | c1 :: cs ->
      (Char.lowercase_ascii c1) :: (to_snake_case cs)
    | [] -> []
  in
  to_list ident
  |> to_snake_case
  |> to_string
  |> String.capitalize_ascii


let field_name (field_name : string option) =
  match String.uncapitalize_ascii (Option.value_exn field_name) with
  | name when is_reserved name -> name ^ "'"
  | name -> name

let variant_name name = module_name name

let constructor_name (name : string option) =
  String.capitalize_ascii (Option.value_exn name)
