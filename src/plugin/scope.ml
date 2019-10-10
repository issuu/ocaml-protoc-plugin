open StdLabels
open MoreLabels

module StringMap = Map.Make(String)

type t = { path: string list;
           type_db: string StringMap.t }

let module_name_of_proto file =
  Filename.chop_extension file |> String.capitalize_ascii

(* Create a list of types *)
let rec map_types ~path ~message_types ~enum_types =
  let type_of_enum: Spec.Descriptor.enum_descriptor_proto -> string list = fun {name; _ } ->
    let name = Option.value_exn name in
    name :: path
  in
  let types_of_nested_types: Spec.Descriptor.descriptor_proto -> string list list = fun { name; nested_type = message_types; enum_type = enum_types; _ } ->
    let name = Option.value_exn name in
    let path = (name :: path) in
    path :: map_types ~path ~message_types ~enum_types
  in
  let enum_types = List.map ~f:type_of_enum enum_types in
  let message_types =
    List.map ~f:types_of_nested_types message_types
    |> List.concat
  in
  enum_types @ message_types

let make_type_db: Spec.Descriptor.file_descriptor_proto list -> string StringMap.t = fun descriptions ->
  let types_of_file Spec.Descriptor.{ name; message_type = message_types; enum_type = enum_types; package; _ } =
    let path =
      Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package
      |> List.rev
    in
    let module_name = Option.value_exn name |> module_name_of_proto in
    (module_name, map_types ~path ~message_types ~enum_types:enum_types)
  in
  let types = List.map ~f:types_of_file descriptions in
  (* Construct a map *)
  List.fold_left ~init:StringMap.empty ~f:(fun acc (module_name, types) ->
    List.fold_left ~init:acc ~f:(fun acc type_ ->
      let type_name = "" :: (List.rev type_) |> String.concat ~sep:"." in
      StringMap.add ~key:type_name ~data:module_name acc
    ) types
  ) types

let init files =
  let type_db = make_type_db files in
  { path = []; type_db }

let push: t -> string -> t = fun t name -> { t with path = name :: t.path }

let pop: t -> string -> t = fun t name ->
  match t.path with
  | p :: ps when String.equal p name -> { t with path = ps }
  | [] -> failwith "Cannot pop empty scope"
  | _ -> failwith "Cannot pop wrong scope"

let get_scoped_name ?postfix t = function
  | Some name -> begin
      let module_name = match StringMap.find_opt name t.type_db with
        | Some x -> x
        | None -> (* Dump the type DB *)
            StringMap.iter ~f:(fun ~key ~data -> Printf.eprintf "     %s -> %s\n%!" key data) t.type_db;
            failwith ("Could not locate type in database: " ^ name)
      in
      match String.split_on_char ~sep:'.' name with
      | "" :: xs ->
        let rec inner = function
          | x :: xs, y :: ys when String.equal (String.lowercase_ascii x) (String.lowercase_ascii y) -> inner (xs, ys)
          | xs, _ ->
            List.map ~f:String.capitalize_ascii xs
            |> (fun stem -> Option.value_map ~default:stem ~f:(fun p -> stem @ [p]) postfix)
            |> String.concat ~sep:"."
        in
        inner (module_name :: xs, List.rev t.path)
      | _ -> failwith "Expected name to start with a '.'"
    end
  | None -> failwith "Does not contain a name"

let get_current_scope t = String.concat ~sep:"." (List.rev t.path)

let in_current_scope t name =
  String.equal (get_scoped_name t (Some name)) ""
