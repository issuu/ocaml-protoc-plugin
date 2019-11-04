open StdLabels
open MoreLabels

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

open Spec.Descriptor.Google.Protobuf

type element = { module_name: string; ocaml_name: string; cyclic: bool; recursive: bool }
type t = { module_name: string;
           package_depth: int;
           proto_path: string;
           type_db: element StringMap.t }

let dump_type_map type_map =
  Printf.eprintf "Type map:\n";
  StringMap.iter ~f:(fun ~key ~data:{module_name; ocaml_name; cyclic; recursive} ->
      Printf.eprintf "     %s -> %s#%s, C:%b, R:%b\n%!" key module_name ocaml_name cyclic recursive
    ) type_map;
  Printf.eprintf "Type map end:\n%!"

let _ = dump_type_map

let module_name_of_proto file =
  (* Strip the path *)
  Filename.chop_extension file |> Filename.basename |> String.capitalize_ascii

let make_type_map: FileDescriptorProto.t list -> element StringMap.t = fun descriptions ->
  let make_proto_name path name =
    path ^ name
  in

  let make_ocaml_name name_set path ~f name =
    let name = match path with
      | path -> path ^ (f name)
    in
    let rec inner name =
      match StringSet.mem name name_set with
      | true -> inner (name ^ "'")
      | false -> name
    in
    let ocaml_name = inner name in
    (StringSet.add ocaml_name name_set, ocaml_name)
  in
  (* Need to handle extensions, as they will also get a name *)
  let map_enum ~ocaml_name ~name_set ~proto_path ~ocaml_path EnumDescriptorProto.{ name; _ } =
    let name = Option.value_exn ~message:"All enums must have a name" name in
    let proto_name = make_proto_name proto_path name in
    let name_set, ocaml_name = make_ocaml_name name_set ocaml_path ~f:ocaml_name name in
    (name_set, [proto_name, ocaml_name, []])
  in

  let rec map_message ~ocaml_name ~name_set ~proto_path ~ocaml_path ~name ~fields ~messages ~enums =
    let name_set, proto_path, ocaml_path, elem = match name with
      | Some name ->
        let proto_name = make_proto_name proto_path name in
        let name_set, ocaml_name = make_ocaml_name name_set ocaml_path ~f:ocaml_name name in
        let proto_path = proto_name ^ "." in
        let ocaml_path = ocaml_name ^ "." in
        let fields = List.fold_left ~init:[] ~f:(fun acc -> function
            | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } -> type_name :: acc
            | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } -> type_name :: acc
            | _ -> acc
          ) fields
        in
        (name_set, proto_path, ocaml_path, [(proto_name, ocaml_name, fields)])
      | None -> (name_set, proto_path, ocaml_path, [])
    in
    let (name_set, types) =
      List.fold_left ~init:(name_set, elem) ~f:(fun (name_set, acc) DescriptorProto.{ name; field; nested_type; enum_type; _} ->
          let (name_map, types) = map_message ~ocaml_name ~name_set ~proto_path ~ocaml_path ~name ~fields:field ~messages:nested_type ~enums:enum_type in
          (name_map, List.rev_append acc types)
        ) messages
    in
    let (name_map, types) =
      List.fold_left ~init:(name_set, types) ~f:(fun (name_set, acc) descriptor ->
          let (name_map, types) = map_enum ~ocaml_name ~name_set ~proto_path ~ocaml_path descriptor in
          (name_map, List.rev_append acc types)
        ) enums
    in
    (name_map, types)
  in

  (* Based on the options in the FileDescriptorProto, we select the proto_name -> ocaml_name function *)
  let make_map FileDescriptorProto.{ name; message_type = messages; package; enum_type = enums; options; _ } =
    let mk_ocaml_name =
      let use_snakecase =
        Option.bind ~f:(fun option ->
            let use_snakecase =
              Spec.Options.Ocaml_options.get option
              |> Ocaml_protoc_plugin.Result.get ~msg:"Could not parse ocaml options"
            in
            use_snakecase) options
        |> Option.value ~default:false
      in
      match use_snakecase with
      | false -> Names.module_name
      | true -> fun name -> Names.to_snake_case name |> Names.module_name
    in
    let file_name =
      Option.value_exn ~message:"All files must have a name" name
      |> module_name_of_proto
    in
    let proto_path = Option.value_map ~default:"." ~f:(fun p -> "." ^ p ^ ".") package in
    let ocaml_path = Option.value_map ~default:"" ~f:(fun p ->
        String.split_on_char ~sep:'.' p
        |> List.map ~f:mk_ocaml_name
        |> String.concat ~sep:"."
        |> Printf.sprintf "%s."
      ) package
    in
    let name_set = StringSet.singleton ocaml_path in
    let (_name_map, types) = map_message ~ocaml_name:mk_ocaml_name ~name_set ~proto_path ~ocaml_path ~name:None ~fields:[] ~messages ~enums in
    List.fold_left ~init:StringMap.empty ~f:(fun acc (proto_name, ocaml_name, references) ->
        StringMap.add ~key:proto_name ~data:(ocaml_name, file_name, references) acc
      ) types
  in

  let is_cyclic type_map name =
    let rec inner seen name =
      let (_ocaml_name, _file_name, references) = StringMap.find name type_map in
      let unseen = List.filter ~f:(fun name -> StringSet.mem name seen |> not) references in
      let seen = List.fold_left ~init:seen ~f:(fun acc name -> StringSet.add name acc) unseen in
      List.fold_left ~init:seen ~f:inner unseen
    in
    let seen = inner StringSet.empty name in
    StringSet.mem name seen
  in

  let is_recursive type_map name =
    let (_, _, references) = StringMap.find name type_map in
    List.mem name ~set:references
  in

  let type_map =
    List.map ~f:make_map descriptions
    |> List.fold_left ~init:StringMap.empty ~f:(
      StringMap.merge ~f:(fun _name -> function
          | Some a -> fun _ -> Some a
          | None -> fun x -> x
        )
    )
  in
  StringMap.mapi ~f:(fun name (ocaml_name, file_name, _references) ->
      { ocaml_name; module_name = file_name; cyclic = is_cyclic type_map name; recursive = is_recursive type_map name}
    ) type_map

let init files =
  let type_db = make_type_map files in
  { module_name = ""; proto_path = ""; package_depth = 0; type_db }

let for_descriptor t FileDescriptorProto.{ name; package; _ } =
  let name = Option.value_exn ~message:"All file descriptors must have a name" name in
  let module_name = module_name_of_proto name in
  let package_depth = Option.value_map ~default:0 ~f:(fun p -> String.split_on_char ~sep:'.' p |> List.length) package in
  { t with package_depth; module_name; proto_path = ""}

let push: t -> string -> t = fun t name -> { t with proto_path = t.proto_path ^ "." ^ name }

let rec drop n = function
  | [] -> []
  | _ :: xs when n > 0 -> drop (n - 1) xs
  | xs -> xs

let get_scoped_name ?postfix t name =
  let name = Option.value_exn ~message:"Does not contain a name" name in

  let { ocaml_name; module_name; _ } = StringMap.find name t.type_db in
  let type_name = match String.equal module_name t.module_name with
    | true ->
      ocaml_name
      |> String.split_on_char ~sep:'.'
      |> drop t.package_depth
      |> String.concat ~sep:"."
    | false -> module_name ^ "." ^ ocaml_name
  in
  (* Strip away the package depth *)
  Option.value_map ~default:type_name ~f:(fun postfix -> type_name ^ "." ^ postfix) postfix

(* Need a function to get the module name based on the path *)
let get_message_name t name =
  let name = Option.value_exn ~message:"Does not contain a name" name in
  (* Find the correct mapping *)
  let proto_name = t.proto_path ^ "." ^ name in
  let { ocaml_name; _ } = StringMap.find proto_name t.type_db in
  (* Let the last part of the ocaml name *)
  String.split_on_char ~sep:'.' ocaml_name
  |> List.rev
  |> List.hd

let get_current_scope t =
  let { module_name; ocaml_name = _; _ } = StringMap.find t.proto_path t.type_db in
  (String.lowercase_ascii module_name) ^ t.proto_path

let is_cyclic t =
  let { cyclic; _ } = StringMap.find t.proto_path t.type_db in
  cyclic

let is_recursive t =
  let { recursive; _ } = StringMap.find t.proto_path t.type_db in
  recursive
