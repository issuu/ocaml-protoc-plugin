open StdLabels

type t = {
  annot: string;
  opens: string list;
  int64_as_int: bool;
  int32_as_int: bool;
  fixed_as_int: bool;
  debug: bool;
  singleton_record: bool;
}

let default = {
  annot = "";
  opens = [];
  int64_as_int = true;
  int32_as_int = true;
  fixed_as_int = false;
  debug = false;
  singleton_record = false;
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
      | `Expr ("singleton_record", (("true"|"false") as v)) -> { param with singleton_record = (bool_of_string v) };
      | `Stmt "debug" -> { param with debug = true}
      | `Stmt "" -> param
      | _ -> failwith ("Unknown parameter: " ^ option)
    )


let parse_file_options: t -> Spec.Descriptor.Google.Protobuf.FileOptions.t -> t = fun t options ->
  let get ~name ~default v = Ocaml_protoc_plugin.Result.get ~msg:(Printf.sprintf "Could not parse option '%s'" name) v |> Option.value ~default in
  let open Spec.Options in
  let debug = Ocaml_debug.get options |> get ~name:"debug" ~default:t.debug in
  let annot = Ocaml_annot.get options |> get ~name:"annot" ~default:t.annot in
  let opens = Ocaml_opens.get options |> Ocaml_protoc_plugin.Result.get ~msg:"Could not parse option 'opens'" in
  let int64_as_int = Ocaml_int64_as_int.get options |> get ~name:"int64_as_int" ~default:t.int64_as_int in
  let int32_as_int = Ocaml_int32_as_int.get options |> get ~name:"int32_as_int" ~default:t.int32_as_int in
  let fixed_as_int = Ocaml_fixed_as_int.get options |> get ~name:"fixed_as_int" ~default:t.fixed_as_int in
  let singleton_record = Ocaml_singleton_record.get options |> get ~name:"singleton_record" ~default:t.fixed_as_int in
  { debug; annot; opens; int64_as_int; int32_as_int; fixed_as_int; singleton_record }
