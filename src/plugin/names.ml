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
  let name = Option.value_exn name in
  let name = match name with
    | "Ocaml_protoc_plugin" -> "Ocaml_protoc_plugin'"
    | s when s.[0] = '_' -> "P" ^ name ^ "'" (* Change to a name that protobuf cannot create *)
    | _ -> name
  in
  String.capitalize_ascii name

(* Remember to mangle reserved keywords *)
let field_name (field_name : string option) =
  match String.uncapitalize_ascii (Option.value_exn field_name) with
  | name when is_reserved name -> name ^ "'"
  | name -> name

let variant_name name = module_name name

let constructor_name (name : string option) =
  String.capitalize_ascii (Option.value_exn name)
