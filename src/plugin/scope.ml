open StdLabels
open MoreLabels

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

open Spec.Descriptor.Google.Protobuf

(** TODO:
  - Create named for fields. This is needed since we map oneof names in there, so we need to test for collision.
  - Oneofs might also create collisions, so we need to create a full map here.
  - Service names should also be mangled
*)

type element = { module_name: string; ocaml_name: string; cyclic: bool; field_names: string StringMap.t }

let module_name_of_proto file =
  Filename.chop_extension file |> Filename.basename |> String.capitalize_ascii

module Type_tree = struct
  (** Need to hold oneofs, and field_names. We could also stuff oneofs in here *)
  type t = { name: string; types: t list; depends: string list; field_names: string list }
  type file = { module_name: string; types: t list }

  let compare { name = n1; _ } { name = n2; _ } = String.compare n1 n2

  let map_enum EnumDescriptorProto.{ name; _ } =
    let name = Option.value_exn ~message:"All enums must have a name" name in
    { name; types = []; depends = []; field_names = [] }

  let rec map_message DescriptorProto.{ name; field = fields; nested_type = nested_types; enum_type = enums; oneof_decl; _} : t =
    let name = Option.value_exn ~message:"All messages must have a name" name in
    let depends =
      List.fold_left ~init:[] ~f:(fun acc -> function
          | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_MESSAGE; _ } ->
            type_name :: acc
          | FieldDescriptorProto.{ type_name = Some type_name; type' = Some Type.TYPE_ENUM; _ } ->
            type_name :: acc
          | _ -> acc
      ) fields
    in
    let enums = List.map ~f:map_enum enums in
    let nested_types = List.map ~f:map_message nested_types in
    let types = List.sort ~cmp:compare (List.rev_append enums nested_types) in
    let field_names =
      let acc = List.fold_left ~init:[] ~f:(fun acc -> function
          | FieldDescriptorProto.{ name = Some name; oneof_index = None; _ } -> name :: acc
          | _ -> acc
        ) fields
      in
      List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } ->
          (Option.value_exn ~message:"Oneof names cannot be null" name) :: acc
        ) oneof_decl
    in
    { name; types; depends; field_names }

  let map_file FileDescriptorProto.{ name; message_type = messages; package; enum_type = enums; options = _; _ } =
    let messages = List.map ~f:map_message messages in
    let enums = List.map ~f:map_enum enums in
    let types = enums @ messages in
    let module_name = Option.value_exn ~message:"File descriptor must have a name" name in
    let packages = Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package in
    let types = List.fold_right ~init:types ~f:(fun name types -> [ { name; types; depends = []; field_names = [] } ]) packages in
    { module_name; types }

  let create_cyclic_map { module_name = _ ; types } =
    let rec traverse path map { name; types; depends; _ } =
      let path = path ^ "." ^ name in
      let map = StringMap.add ~key:path ~data:(StringSet.of_list depends) map in
      List.fold_left ~init:map ~f:(traverse path) types
    in
    let is_cyclic map name =
      let rec inner name (seen : StringSet.t) =
        (* If a type has more than one depend, then the cyclic chain is broken, and
           we can stop processing further *)
        match StringMap.find_opt name map with
        | None -> seen
        | Some depends when StringSet.cardinal depends = 1 ->
          let unseen = StringSet.diff depends seen in
          let seen = StringSet.union depends seen in
          StringSet.fold ~init:seen ~f:inner unseen
        | Some _ -> seen
      in
      let seen = inner name StringSet.empty in
      StringSet.mem name seen
    in
    let map = List.fold_left ~init:StringMap.empty ~f:(traverse "") types in
    StringMap.mapi ~f:(fun name _ -> is_cyclic map name) map

  (* Create a map: proto_name -> ocaml_name.
     Mapping is done in two passes:
     The first pass uses the standard name mapping function, to preserve names which maps to a standard type.
     The second pass does the actual mapping, while prioritizing the standard mapping in case of name collision.
     The result is a map: proto_name -> mangled name.
  *)
  let create_name_map ~standard_f ~mangle_f names =
    (* Names where standard_f and mangle_f produce the same name
       gets priority. So we create a pre-allocation map here *)
    let standard_name_map =
      List.fold_left ~init:StringMap.empty ~f:(fun map name ->
        let mangle_name = mangle_f name in
        let standard_name = standard_f name in
        match String.equal mangle_name standard_name with
          | true -> StringMap.add ~key:mangle_name ~data:name map
          | false -> map
      ) names
    in
    let rec uniq_name names ocaml_name =
      match List.assoc_opt ocaml_name names with
        | None -> ocaml_name
        | Some _ -> uniq_name names (ocaml_name ^ "'")
    in
    List.fold_left ~init:[] ~f:(fun names proto_name ->
      let ocaml_name = mangle_f proto_name in
      let ocaml_name =
        match StringMap.find_opt proto_name standard_name_map with
          | Some name when String.equal name proto_name -> ocaml_name
          | Some _ -> ocaml_name ^ "_"
          | None -> ocaml_name
      in
      (uniq_name names ocaml_name, proto_name) :: names
    ) names
    |> List.fold_left ~init:StringMap.empty ~f:(fun map (ocaml_name, proto_name) ->
        StringMap.add ~key:proto_name ~data:ocaml_name map
      )

  (* Create a type db: map proto-type -> { module_name, ocaml_name, is_cyclic } *)
  let create_file_db ~mangle_f cyclic_map { module_name; types } =
    let module_name = module_name_of_proto module_name in
    let rec traverse_type map path types =
      let inner ~map ~name_map path { name; types; field_names; _ } =
        let ocaml_name =
          let ocaml_name = StringMap.find name name_map in
          match StringMap.find path map with
          | { ocaml_name = ""; _ } -> ocaml_name
          | { ocaml_name = path; _ } -> path ^ "." ^ ocaml_name
        in
        let path = path ^ "." ^ name in
        let cyclic = StringMap.find path cyclic_map in
        let field_names =
          create_name_map
            ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
            ~mangle_f:(Names.field_name ~mangle_f)
            field_names
        in
        let data = { module_name; ocaml_name; cyclic; field_names } in
        let map = StringMap.add ~key:path ~data map in
        traverse_type map path types
      in
      let name_map =
        List.map ~f:(fun { name; _ } -> name) types
        |> create_name_map
          ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
          ~mangle_f:(Names.module_name ~mangle_f)
      in
      List.fold_left ~init:map ~f:(fun map type_-> inner ~map ~name_map path type_) types
    in
    let map = StringMap.singleton "" { ocaml_name = ""; module_name; cyclic = false; field_names = StringMap.empty } in
    traverse_type map "" types

  let option_snake_case FileDescriptorProto.{ options; _ } =
    Option.map ~f:Spec.Options.Ocaml_options.get options
    |> function
    | Some (Ok (Some v)) -> v
    | Some (Ok None) -> false
    | None -> false
    | Some (Error _e) -> failwith "Fas"


  let create_db (files : FileDescriptorProto.t list)=
    let inner proto_file =
      let mangle_f = match option_snake_case proto_file with
        | true -> Names.to_snake_case
        | false -> fun x -> x
      in
      let map = map_file proto_file in
      let cyclic_map = create_cyclic_map map in
      let file_db = create_file_db ~mangle_f cyclic_map map in
      file_db
    in
    List.map ~f:inner files
    |> List.fold_left ~init:StringMap.empty ~f:(
      StringMap.merge ~f:(fun _ a -> function
          | None -> a
          | b -> b
        )
    )
end


type t = { module_name: string;
           package_depth: int;
           proto_path: string;
           type_db: element StringMap.t }

let dump_type_map type_map =
  Printf.eprintf "Type map:\n";
  StringMap.iter ~f:(fun ~key ~data:{module_name; ocaml_name; cyclic; _ } ->
      Printf.eprintf "     %s -> %s#%s, C:%b\n%!" key module_name ocaml_name cyclic
    ) type_map;
  Printf.eprintf "Type map end:\n%!"

let _ = dump_type_map


let init files =
  let type_db = Type_tree.create_db files in
  if false then dump_type_map type_db;
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

let get_field_name t name =
  let { field_names; _ } = StringMap.find t.proto_path t.type_db in
  match StringMap.find_opt name field_names with
  | None -> failwith (Printf.sprintf "Cannot find %s in %s. [%s]" name t.proto_path (String.concat ~sep:"; " (StringMap.bindings field_names |> List.map ~f:snd)))
  | Some name -> name

let get_current_scope t =
  let { module_name; ocaml_name = _; _ } = StringMap.find t.proto_path t.type_db in
  (String.lowercase_ascii module_name) ^ t.proto_path

let is_cyclic t =
  let { cyclic; _ } = StringMap.find t.proto_path t.type_db in
  cyclic
