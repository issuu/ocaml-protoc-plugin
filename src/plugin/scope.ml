open StdLabels
open MoreLabels

let failwith_f fmt =
  Printf.ksprintf (fun s -> failwith s) fmt

let dump_tree = false
let dump_ocaml_names = false

module StringMap = struct
  include Map.Make(String)

  (** Fail with an error if the key already exists *)
  let add_uniq ~key ~data map =
    update ~key ~f:(function
      | None -> Some data
      | Some _ -> failwith_f "Key %s already exists" key
    ) map
end
module StringSet = Set.Make(String)


(** Module to avoid name clashes in a local scope *)
module Local = struct
  type t = (string, unit) Hashtbl.t
  let init () : t = Hashtbl.create 2
  let get_unique_name t preferred_name =
    let rec inner name =
      match Hashtbl.mem t name with
      | true -> inner (name ^ "'")
      | false when Names.is_reserved name -> inner (name ^ "'")
      | false -> name

   in
   let name = inner preferred_name in
   Hashtbl.add t ~key:name ~data:();
   name
end


open Spec.Descriptor.Google.Protobuf

type element = { module_name: string; ocaml_name: string; cyclic: bool }

let import_module_name = "Imported'modules"

let module_name_of_proto file =
  Filename.chop_extension file
  |> Filename.basename
  |> String.capitalize_ascii
  |> String.map ~f:(function '-' -> '_' | c -> c)

let has_mangle_option options =
  Option.map ~f:Spec.Options.Ocaml_options.get options
  |> function
  | Some (Ok (Some v)) -> v
  | Some (Ok None) -> false
  | None -> false
  | Some (Error _e) -> failwith "Could not parse ocaml-protoc-plugin options with id 1074"

module Type_tree = struct
  type t = { name: string; types: t list; depends: string list; fields: string list * string list list; enum_names: string list; service_names: string list }
  type file = { module_name: string; types: t list }

  let map_enum EnumDescriptorProto.{ name; value = values; _ } =
    let name = Option.value_exn ~message:"All enums must have a name" name in
    let enum_names =
      List.map ~f:(fun EnumValueDescriptorProto.{ name; _ } ->
        Option.value_exn ~message:"All enum values must have a name" name
      ) values
    in
    { name; types = []; depends = []; fields = [], []; enum_names; service_names = [] }

  let map_service ServiceDescriptorProto.{ name; method' = methods; _ } =
    let name = Option.value_exn ~message:"All enums must have a name" name in
    let service_names =
      List.map ~f:(fun MethodDescriptorProto.{ name; _ } ->
        Option.value_exn ~message:"All service methods must have a name" name
      ) methods
    in
    { name; types = []; depends = []; fields = [], []; enum_names = []; service_names}

  let map_extension FieldDescriptorProto.{ name; _ } =
    let name = Option.value_exn ~message:"All enums must have a name" name in
    { name; types = []; depends = []; fields = [], []; enum_names = []; service_names = []}

  let split_oneof_fields fields =
    let rec group acc ~eq = function
      | [] when acc = [] -> []
      | [] -> [ List.rev acc ]
      | x1 :: (x2 :: _  as xs) when eq x1 x2 -> group (x1 :: acc) ~eq xs
      | x :: xs -> (List.rev (x :: acc)) :: group [] ~eq xs
    in
    let field_number_of_field = function
      | FieldDescriptorProto.{ oneof_index = None; _ } -> failwith "Only oneof fields here"
      | FieldDescriptorProto.{ oneof_index = Some number; _ } -> number
    in

    let fields = List.sort ~cmp:(fun a b -> compare (field_number_of_field a) (field_number_of_field b)) fields in
    group [] ~eq:(fun a b -> field_number_of_field a = field_number_of_field b) fields


  let rec map_message DescriptorProto.{ name; field = fields; nested_type = nested_types; enum_type = enums; oneof_decl = oneof_decls; extension = extensions; _} : t =
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
    let extensions = List.map ~f:map_extension extensions in
    let nested_types = List.map ~f:map_message nested_types in
    let types = List.sort ~cmp:compare (enums @ extensions @ nested_types) in
    let fields =
      let field_name FieldDescriptorProto.{ name; _} =
        Option.value_exn ~message:"Field names cannot be null" name
      in
      let (plain_fields, oneof_fields) = List.partition ~f:(function FieldDescriptorProto.{ proto3_optional = Some true; _ } -> true
                                                                   | { oneof_index = None; _ } -> true
                                                                   | _ -> false) fields in
      let plain_fields =
        let acc = List.map ~f:field_name plain_fields in
        List.fold_left ~init:acc ~f:(fun acc OneofDescriptorProto.{ name; _ } ->
          (Option.value_exn ~message:"Oneof names cannot be null" name) :: acc
        ) oneof_decls
      in
      let oneof_fields =
        split_oneof_fields oneof_fields
        |> List.map ~f:(List.map ~f:field_name)
      in
      plain_fields, oneof_fields
    in
    { name; types; depends; fields; enum_names = []; service_names = [] }

  let map_file FileDescriptorProto.{ name; message_type = messages; package; enum_type = enums; service = services; extension = extensions; _ } =
    let messages = List.map ~f:map_message messages in
    let enums = List.map ~f:map_enum enums in
    let services = List.map ~f:map_service services in
    let extensions = List.map ~f:map_extension extensions in
    let types = enums @ messages @ services @ extensions in
    let module_name = Option.value_exn ~message:"File descriptor must have a name" name in
    let packages = Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package in
    let types = List.fold_right ~init:types ~f:(fun name types -> [ { name; types; depends = []; fields = [], []; enum_names = []; service_names = [] } ]) packages in
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

  (** Create a map: proto_name -> ocaml_name.
      Mapping is done in multiple passes to prioritize which mapping wins in case of name clashes
  *)
  let create_name_map ~standard_f ~mangle_f names =
    let rec uniq_name names ocaml_name =
      match List.assoc_opt ocaml_name names with
        | None -> ocaml_name
        | Some _ -> uniq_name names (ocaml_name ^ "'")
    in
    let names =
      List.map ~f:(fun name ->
        let mangle_name = mangle_f name in
        let standard_name = standard_f name in
        (name, mangle_name, standard_name)
      ) names
    in
    let standard_name_map =
      let inject ~f map =
        List.fold_left ~init:map ~f:(fun map (name, mangled_name, standard_name) ->
          match f name mangled_name standard_name with
            | true when StringMap.mem mangled_name map -> map
            | true -> StringMap.add ~key:mangled_name ~data:name map
            | false -> map
        ) names
      in
      StringMap.empty
      |> inject ~f:(fun name mangled_name _standard_name -> String.equal mangled_name name)
      |> inject ~f:(fun _name mangled_name standard_name -> String.equal mangled_name standard_name)
      |> inject ~f:(fun name mangled_name _standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii name))
      |> inject ~f:(fun _name mangled_name standard_name -> String.equal (String.lowercase_ascii mangled_name) (String.lowercase_ascii standard_name))
    in
    List.fold_left ~init:[] ~f:(fun names (proto_name, ocaml_name, _) ->
      let ocaml_name =
        match StringMap.find_opt ocaml_name standard_name_map with
          | Some name when String.equal name proto_name -> ocaml_name
          | Some _ -> ocaml_name ^ "'"
          | None -> ocaml_name
      in
      (uniq_name names ocaml_name, proto_name) :: names
    ) names
    |> List.fold_left ~init:StringMap.empty ~f:(fun map (ocaml_name, proto_name) ->
        StringMap.add ~key:proto_name ~data:ocaml_name map
      )

  (** Create a type db: map proto-type -> { module_name, ocaml_name, is_cyclic } *)
  let create_file_db ~mangle cyclic_map { module_name; types } =
    let mangle_f = match mangle with
      | true -> Names.to_snake_case
      | false -> fun x -> x
    in
    let module_name = module_name_of_proto module_name in
    let add_names ~path ~ocaml_name map names =
      StringMap.fold ~init:map ~f:(fun ~key ~data map ->
          StringMap.add_uniq
            ~key:(path ^ "." ^ key)
            ~data:{ module_name; ocaml_name = ocaml_name ^ "." ^ data; cyclic = false }
            map
        ) names
    in

    let rec traverse_types map path types =
      let map_type ~map ~name_map path { name; types; fields = (plain_fields, oneof_fields); enum_names; service_names; _} =
        let ocaml_name =
          let ocaml_name = StringMap.find name name_map in
          match StringMap.find path map with
          | { ocaml_name = ""; _ } -> ocaml_name
          | { ocaml_name = path; _ } -> path ^ "." ^ ocaml_name
        in
        let path = path ^ "." ^ name in
        let cyclic = StringMap.find path cyclic_map in
        let map =
          create_name_map
            ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
            ~mangle_f:(Names.field_name ~mangle_f)
            plain_fields
          |> add_names ~path ~ocaml_name map
        in
        let map =
          List.fold_left ~init:map ~f:(fun map fields ->
            create_name_map
              ~standard_f:(Names.poly_constructor_name ~mangle_f:(fun x -> x))
              ~mangle_f:(Names.poly_constructor_name ~mangle_f)
              fields
            |> add_names ~path ~ocaml_name map
          ) oneof_fields
        in
        let map =
          create_name_map
            ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
            ~mangle_f:(Names.module_name ~mangle_f)
            enum_names
          |> add_names ~path ~ocaml_name map
        in

        let map =
          create_name_map
            ~standard_f:(Names.field_name ~mangle_f:(fun x -> x))
            ~mangle_f:(Names.field_name ~mangle_f)
            service_names
          |> add_names ~path ~ocaml_name map
        in
        let map = StringMap.add_uniq ~key:path ~data:{ module_name; ocaml_name; cyclic } map in

        traverse_types map path types
      in
      let name_map =
        List.map ~f:(fun { name; _ } -> name) types
        |> create_name_map
          ~standard_f:(Names.module_name ~mangle_f:(fun x -> x))
          ~mangle_f:(Names.module_name ~mangle_f)
      in
      List.fold_left ~init:map ~f:(fun map type_ -> map_type ~map ~name_map path type_) types
    in

    let map = StringMap.singleton "" { ocaml_name = ""; module_name; cyclic = false } in
    traverse_types map "" types

  let create_db (files : FileDescriptorProto.t list)=
    let inner proto_file =
      let map = map_file proto_file in
      let cyclic_map = create_cyclic_map map in
      let file_db = create_file_db ~mangle:(has_mangle_option proto_file.options) cyclic_map map in
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
           type_db: element StringMap.t;
           ocaml_names: StringSet.t;
         }

let dump_type_map type_map =
  Printf.eprintf "Type map:\n";
  StringMap.iter ~f:(fun ~key ~data:{module_name; ocaml_name; cyclic; _ } ->
      Printf.eprintf "     %s -> %s#%s, C:%b\n%!" key module_name ocaml_name cyclic
    ) type_map;
  Printf.eprintf "Type map end:\n%!"

let init files =
  let type_db = Type_tree.create_db files in
  let ocaml_names =
    StringMap.fold ~init:StringSet.empty
      ~f:(fun ~key:_ ~data:{ocaml_name; _} acc ->
      StringSet.add ocaml_name acc
    ) type_db
  in
  if dump_tree then dump_type_map type_db;
  if dump_ocaml_names then
    StringSet.iter ~f:(Printf.eprintf "%s\n") ocaml_names;


  { module_name = ""; proto_path = ""; package_depth = 0; type_db; ocaml_names}

let for_descriptor t FileDescriptorProto.{ name; package; _ } =
  let name = Option.value_exn ~message:"All file descriptors must have a name" name in
  let module_name = module_name_of_proto name in
  let package_depth = Option.value_map ~default:0 ~f:(fun p -> String.split_on_char ~sep:'.' p |> List.length) package in
  { t with package_depth; module_name; proto_path = "" }

let push: t -> string -> t = fun t name -> { t with proto_path = t.proto_path ^ "." ^ name }

let get_scoped_name ?postfix t name =
  (* Take the first n elements from the list *)
  let take n l =
    let rec inner = function
      | (0, _) -> []
      | (_, []) -> []
      | (n, x :: xs) -> x :: inner (n - 1, xs)
    in
    inner (n, l)
  in

  (* Resolve name in the current context and return the fully qualified module name,
     iff exists *)
  let resolve t name =
    let rec lookup name = function
      | path ->
        begin
          let path_str = String.concat ~sep:"." (name :: path |> List.rev) in
          match StringSet.mem path_str t.ocaml_names with
          | false -> begin
              match path with
              | [] -> None
              | _ :: ps -> lookup name ps
            end
          | true -> Some path_str
        end
    in
    let { ocaml_name = ocaml_path; _ } =
      StringMap.find t.proto_path t.type_db
    in
    let path = match String.equal "" ocaml_path with
      | false -> String.split_on_char ~sep:'.' ocaml_path |> List.rev
      | true -> []
    in
    lookup name path
  in

  let name = Option.value_exn ~message:"Does not contain a name" name in
  let { ocaml_name; module_name; _ } = StringMap.find name t.type_db in

  (* Lookup a fully qualified name in the current scope.
     Returns the shortest name for the type in the current scope *)
  let rec lookup postfix_length = function
    | p :: ps ->
      begin
        let expect = String.concat ~sep:"." (List.rev (p :: ps)) in
        let resolve_res = resolve t p in
        match resolve_res with
        | Some path when String.equal path expect ->
          let how_many = postfix_length in
          let ocaml_name =
            String.split_on_char ~sep:'.' ocaml_name
            |> List.rev
            |> take how_many
            |> List.rev
            |> String.concat ~sep:"."
          in
          ocaml_name
        | _ ->
          lookup (postfix_length + 1) ps
      end
    | [] ->
      failwith_f "Unable to reference '%s'. This is due to a limitation in the Ocaml mappings. To work around this limitation make sure to use a unique package name" name
  in
  let type_name =
    match String.equal module_name t.module_name with
    | true ->
      let names =
        String.split_on_char ~sep:'.' ocaml_name
        |> List.rev
      in
      lookup 1 names
    | false ->
      Printf.sprintf "%s.%s.%s" import_module_name module_name ocaml_name
  in

  match postfix, type_name with
  | Some postfix, "" -> postfix
  | None, "" -> failwith "Empty type cannot be referenced"
  | None, type_name -> type_name
  | Some postfix, type_name -> Printf.sprintf "%s.%s" type_name postfix

let get_name t name =
  let path = t.proto_path ^ "." ^ name in
  match StringMap.find_opt path t.type_db with
    | Some { ocaml_name; _ } -> String.split_on_char ~sep:'.' ocaml_name |> List.rev |> List.hd
    | None -> failwith (Printf.sprintf "Cannot find %s in %s." name t.proto_path)

let get_name_exn t name =
  let name = Option.value_exn ~message:"Does not contain a name" name in
  get_name t name

let get_current_scope t =
  let { module_name; ocaml_name = _; _ } = StringMap.find t.proto_path t.type_db in
  (String.lowercase_ascii module_name) ^ t.proto_path

let get_package_name { proto_path; _ } =
  match String.split_on_char ~sep:'.' proto_path with
  | _ :: xs -> List.rev xs |> List.tl |> List.rev |> String.concat ~sep:"." |> Option.some
  | _ -> None

let is_cyclic t =
  let { cyclic; _ } = StringMap.find t.proto_path t.type_db in
  cyclic
