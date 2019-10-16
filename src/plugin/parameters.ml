open StdLabels

type t = {
  annot: string;
  opens: string list;
  int64_as_int: bool;
  int32_as_int: bool;
  fixed_as_int: bool;
  debug: bool;
}

let default = {
  annot = "";
  opens = [];
  int64_as_int = true;
  int32_as_int = true;
  fixed_as_int = false;
  debug = false;
}

let parse_option str =
  match String.index str '=' with
  | n -> `Expr (String.sub str ~pos:0 ~len:n, String.sub str ~pos:(n + 1) ~len:(String.length str - n - 1))
  | exception Not_found -> `Stmt str

let parse parameters =
  String.split_on_char ~sep:';' parameters
  |> List.fold_left ~init:default ~f:(fun param option ->
      match parse_option option with
      | `Expr ("annot", annot) -> { param with annot }
      | `Expr ("open", open') -> { param with opens = default.opens @ [open'] }
      | `Stmt "use_int32" -> { param with int32_as_int = false }
      | `Stmt "use_int64" -> { param with int64_as_int = false }
      | `Expr ("fixed_as_int", (("true"|"false") as v)) -> { param with fixed_as_int = (bool_of_string v) };
      | `Expr ("int64_as_int", (("true"|"false") as v)) -> { param with int64_as_int = (bool_of_string v) };
      | `Expr ("int32_as_int", (("true"|"false") as v)) -> { param with int32_as_int = (bool_of_string v) };
      | `Stmt "debug" -> { param with debug = true}
      | _ -> failwith ("Unknown parameter: " ^ option)
    )
