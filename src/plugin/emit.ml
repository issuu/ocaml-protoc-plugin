open StdLabels
open Parameters
open Spec.Descriptor.Google.Protobuf

module IntSet = Set.Make(struct type t = int let compare = compare end)
let sprintf = Printf.sprintf

(** Slightly overloaded name here.
    Its also used for all other types which would go into a module *)
type module' = {
  module_name : string;
  signature : Code.t;
  implementation : Code.t;
}

(* Enums are not mangled - Maybe they should be lowercased though. *)
let emit_enum_type ~scope ~params
    EnumDescriptorProto.{name; value = values; options = _; reserved_range = _; reserved_name = _}
    : module' =
  let name = Option.value_exn ~message:"Enums must have a name" name in
  let module_name = Scope.get_name scope name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let scope = Scope.push scope name in
  let t = Code.init () in
  Code.emit t `None "type t = %s %s"
    (List.map ~f:(fun EnumValueDescriptorProto.{name; _} -> Scope.get_name_exn scope name) values
     |> String.concat ~sep:" | "
    )
    params.Parameters.annot;
  Code.append signature t;
  Code.append implementation t;
  Code.emit signature `None "val to_int: t -> int";
  Code.emit signature `None "val from_int: int -> t Runtime'.Result.t";
  Code.emit signature `None "val from_int_exn: int -> t";

  Code.emit implementation `Begin "let to_int = function";
  List.iter ~f:(fun EnumValueDescriptorProto.{name; number; _} ->
    Code.emit implementation `None "| %s -> %d" (Scope.get_name_exn scope name) (Option.value_exn number)
  ) values;
  Code.emit implementation `End "";
  Code.emit implementation `Begin "let from_int_exn = function";
  let _ =
    List.fold_left ~init:IntSet.empty ~f:(fun seen EnumValueDescriptorProto.{name; number; _} ->
        let idx = (Option.value_exn ~message:"All enum descriptions must have a value" number) in
        match IntSet.mem idx seen with
        | true -> seen
        | false ->
          Code.emit implementation `None "| %d -> %s" idx (Scope.get_name_exn scope name);
          IntSet.add idx seen
      ) values
  in
  Code.emit implementation `None "| n -> Runtime'.Result.raise (`Unknown_enum_value n)";
  Code.emit implementation `End "";
  Code.emit implementation `Begin "let from_int e = Runtime'.Result.catch (fun () -> from_int_exn e)";

  {module_name; signature; implementation}

let emit_service_type ~options scope ServiceDescriptorProto.{ name; method' = methods; _ } =
  let emit_method signature implementation local_scope scope service_name MethodDescriptorProto.{ name; input_type; output_type; _} =
    let name = Option.value_exn name in
    let mangle_f = match Scope.has_mangle_option options with
      | false -> fun id -> id
      | true -> Names.to_snake_case
    in
    let uncapitalized_name = mangle_f name |> String.uncapitalize_ascii |> Scope.Local.get_unique_name local_scope in
    (* To keep symmetry, only ensure that lowercased names are unique
       so that the upper case names are aswell.  We should remove this
       mapping if/when we deprecate the old API *)
    let capitalized_name = String.capitalize_ascii uncapitalized_name in

    let package_name_opt = Scope.get_package_name scope in
    let package_name =
      match package_name_opt with
        | None -> ""
        | Some p -> p ^ "."
    in
    let input = Scope.get_scoped_name scope input_type in
    let input_t = Scope.get_scoped_name scope ~postfix:"t" input_type in
    let output = Scope.get_scoped_name scope output_type in
    let output_t = Scope.get_scoped_name scope ~postfix:"t" output_type in
    let sig_t = sprintf "Runtime'.Service.Rpc with type Request.t = %s and type Response.t = %s" input_t output_t in
    Code.emit implementation `Begin "module %s : %s = struct" capitalized_name sig_t;
    Code.emit implementation `None "let package_name = %s" (Option.value_map ~default:"None" ~f:(fun n -> sprintf "Some \"%s\"" n) package_name_opt);
    Code.emit implementation `None "let service_name = \"%s\"" service_name;
    Code.emit implementation `None "let method_name = \"%s\"" name;
    Code.emit implementation `None "let name = \"/%s%s/%s\"" package_name service_name name;
    Code.emit implementation `None "module Request = %s" input;
    Code.emit implementation `None "module Response = %s" output;
    Code.emit implementation `End "end";
    let sig_t' =
      sprintf "(module Runtime'.Service.Message with type t = %s) * (module Runtime'.Service.Message with type t = %s)" input_t output_t
    in
    Code.emit implementation `Begin "let %s : %s = " uncapitalized_name sig_t';
    Code.emit implementation `None "(module %s : Runtime'.Service.Message with type t = %s ), "
      input
      input_t;
    Code.emit implementation `None "(module %s : Runtime'.Service.Message with type t = %s )"
      output
      output_t;
    Code.emit implementation `End "";

    Code.emit signature `None "module %s : %s" capitalized_name sig_t;
    Code.emit signature `None "val %s : %s" uncapitalized_name sig_t';
    ()
  in
  let name = Option.value_exn ~message:"Service definitions must have a name" name in
  let signature = Code.init () in
  let implementation = Code.init () in
  Code.emit signature `Begin "module %s : sig" (Scope.get_name scope name);
  Code.emit implementation `Begin "module %s = struct" (Scope.get_name scope name);
  let local_scope = Scope.Local.init () in

  List.iter ~f:(emit_method signature implementation local_scope (Scope.push scope name) name) methods;
  Code.emit signature `End "end";
  Code.emit implementation `End "end";
  signature, implementation

let emit_extension ~scope ~params field =
  let FieldDescriptorProto.{ name; extendee; _ } = field in
  let name = Option.value_exn ~message:"Extensions must have a name" name in
  let module_name = (Scope.get_name scope name) in
  let extendee_type = Scope.get_scoped_name scope ~postfix:"t" extendee in
  let extendee_field = Scope.get_scoped_name scope ~postfix:"extensions'" extendee in
  (* Get spec and type *)
  let c =
    let params = Parameters.{params with singleton_record = false} in
    Types.spec_of_field ~params ~syntax:`Proto2 ~scope field
  in
  let signature = Code.init () in
  let implementation = Code.init () in
  Code.append implementation signature;

  Code.emit signature `None "type t = %s %s" c.typestr params.annot;
  Code.emit signature `None "val get_exn: %s -> %s" extendee_type c.typestr;
  Code.emit signature `None "val get: %s -> (%s, [> Runtime'.Result.error]) result" extendee_type c.typestr;
  Code.emit signature `None "val set: %s -> %s -> %s" extendee_type c.typestr extendee_type;

  Code.emit implementation `None "type t = %s %s" c.typestr params.annot;
  Code.emit implementation `None "let get_exn extendee = Runtime'.Extensions.get Runtime'.Deserialize.C.(%s) (extendee.%s)" c.deserialize_spec extendee_field ;
  Code.emit implementation `None "let get extendee = Runtime'.Result.catch (fun () -> get_exn extendee)";
  Code.emit implementation `Begin "let set extendee t =";
  Code.emit implementation `None "let extensions' = Runtime'.Extensions.set Runtime'.Serialize.C.(%s) (extendee.%s) t in" c.serialize_spec extendee_field;
  Code.emit implementation `None "{ extendee with %s = extensions' } [@@warning \"-23\"]" extendee_field;
  Code.emit implementation `End "";
  { module_name; signature; implementation }

let is_map_entry = function
  | Some MessageOptions.{ map_entry = Some true; _ } -> true
  | _ -> false

(** Emit the nested types. *)
let emit_sub dest ~is_implementation ~is_first {module_name; signature; implementation} =
  let () =
    match is_first with
    | true -> Code.emit dest `Begin "module rec %s : sig" module_name
    | false -> Code.emit dest `Begin "and %s : sig" module_name
  in
  Code.append dest signature;
  let () =
    match is_implementation with
    | false -> ()
    | true ->
      Code.emit dest `EndBegin "end = struct ";
      Code.append dest implementation
  in
  Code.emit dest `End "end";
  ()

let rec emit_nested_types ~syntax ~signature ~implementation ?(is_first = true) nested_types =
  match nested_types with
  | [] -> ()
  | sub :: subs ->
    emit_sub ~is_implementation:false signature ~is_first sub;
    emit_sub ~is_implementation:true implementation ~is_first sub;
    emit_nested_types ~syntax ~signature ~implementation ~is_first:false subs

(* Emit a message plus all its subtypes.
   Why is this not being called recursively, but rather calling sub functions which never returns
*)
let rec emit_message ~params ~syntax scope
    DescriptorProto.{ name; field = fields; extension = extensions;
                      nested_type = nested_types; enum_type = enum_types;
                      extension_range = extension_ranges; oneof_decl = oneof_decls; options;
                      reserved_range = _; reserved_name = _ } : module' =

  let signature = Code.init () in
  let implementation = Code.init () in

  let extension_ranges =
    List.map ~f:(function
      | DescriptorProto.ExtensionRange.{ start = Some start; end' = Some end'; _ } -> (start, end')
      | _ -> failwith "Start and end must be defined for Extension ranges"
    ) extension_ranges
  in
  (* Ignore empty modules *)
  let module_name, scope =
    match name with
    | None -> "", scope
    | Some name ->
      let module_name = Scope.get_name scope name in
      module_name, Scope.push scope name
  in
  List.map ~f:(emit_enum_type ~scope ~params) enum_types
  @ List.map ~f:(emit_message ~params ~syntax scope) nested_types
  @ List.map ~f:(emit_extension ~scope ~params) extensions
  |> emit_nested_types ~syntax ~signature ~implementation;

  let () =
    match name with
    | Some _name ->
      let is_map_entry = is_map_entry options in
      let is_cyclic = Scope.is_cyclic scope in
      let Types.{ type'; constructor; apply; deserialize_spec; serialize_spec;
                  default_constructor_sig; default_constructor_impl; merge_impl } =
        Types.make ~params ~syntax ~is_cyclic ~is_map_entry ~extension_ranges ~scope ~fields oneof_decls
      in
      ignore (merge_impl);

      Code.emit signature `None "val name': unit -> string";
      Code.emit signature `None "type t = %s %s" type' params.annot;
      Code.emit signature `None "val make: %s" default_constructor_sig;
      Code.emit signature `None "val merge: t -> t -> t";
      Code.emit signature `None "val to_proto': Runtime'.Writer.t -> t -> Runtime'.Writer.t";
      Code.emit signature `None "val to_proto: t -> Runtime'.Writer.t";
      Code.emit signature `None "val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result";
      Code.emit signature `None "val from_proto_exn: Runtime'.Reader.t -> t";

      Code.emit implementation `None "let name' () = \"%s\"" (Scope.get_current_scope scope);
      Code.emit implementation `None "type t = %s%s" type' params.annot;
      Code.emit implementation `None "let make %s" default_constructor_impl;
      Code.emit implementation `None "let merge = (%s)" merge_impl;

      Code.emit implementation `Begin "let to_proto' =";
      Code.emit implementation `None "let spec = %s in" serialize_spec;
      Code.emit implementation `None "let serialize = Runtime'.Serialize.serialize spec in";
      Code.emit implementation `None "%s" apply;
      Code.emit implementation `End "";

      Code.emit implementation `None "let to_proto t = to_proto' (Runtime'.Writer.init ()) t";

      Code.emit implementation `Begin "let from_proto_exn =";
      Code.emit implementation `None "let constructor = %s in" constructor;
      Code.emit implementation `None "let spec = %s in" deserialize_spec;
      Code.emit implementation `None "Runtime'.Deserialize.deserialize spec constructor";
      Code.emit implementation `End "let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)";
    | None -> ()
  in
  {module_name; signature; implementation}

let rec wrap_packages ~params ~syntax ~options scope message_type services = function
  | [] ->
    let { module_name = _; implementation; signature } = emit_message ~params ~syntax scope message_type in
    List.iter ~f:(fun service ->
      let signature', implementation' = emit_service_type ~options scope service in
      Code.append implementation implementation';
      Code.append signature signature';
      ()
    ) services;
    signature, implementation

  | package :: packages ->
    let signature = Code.init () in
    let implementation = Code.init () in
    let package_name = Scope.get_name scope package in
    let scope = Scope.push scope package in

    let signature', implementation' =
      wrap_packages ~params ~syntax ~options scope message_type services packages
    in

    Code.emit implementation `Begin "module rec %s : sig" package_name;
    Code.append implementation signature';
    Code.emit implementation `EndBegin "end = struct";
    Code.append implementation implementation';
    Code.emit implementation `End "end";
    Code.emit signature `Begin "module rec %s : sig" package_name;
    Code.append signature signature';
    Code.emit signature `End "end";

    signature, implementation

let emit_header implementation ~name ~syntax ~params =
  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(*       AUTOGENERATED FILE - DO NOT EDIT!      *)";
  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(* Generated by: ocaml-protoc-plugin            *)";
  Code.emit implementation `None "(* https://github.com/issuu/ocaml-protoc-plugin *)";
  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(*";
  Code.emit implementation `None "  Source: %s" name;
  Code.emit implementation `None "  Syntax: %s" (match syntax with `Proto2 -> "proto2" | `Proto3 -> "proto3");
  Code.emit implementation `None "  Parameters:";
  Code.emit implementation `None "    debug=%b" params.debug;
  Code.emit implementation `None "    annot='%s'" params.annot;
  Code.emit implementation `None "    opens=[%s]" (String.concat ~sep:"; " params.opens);
  Code.emit implementation `None "    int64_as_int=%b" params.int64_as_int;
  Code.emit implementation `None "    int32_as_int=%b" params.int32_as_int;
  Code.emit implementation `None "    fixed_as_int=%b" params.fixed_as_int;
  Code.emit implementation `None "    singleton_record=%b" params.singleton_record;
  Code.emit implementation `None "*)";
  Code.emit implementation `None "";
  ()

let parse_proto_file ~params scope
    FileDescriptorProto.{ name; package; dependency = dependencies; public_dependency = _;
                          weak_dependency = _; message_type = message_types;
                          enum_type = enum_types; service = services; extension;
                          options; source_code_info = _; syntax; }
  =
  let name = Option.value_exn ~message:"All files must have a name" name |> String.map ~f:(function '-' -> '_' | c -> c) in
  let syntax = match syntax with
    | None | Some "proto2" -> `Proto2
    | Some "proto3" -> `Proto3
    | _ -> failwith "Unsupported syntax"
  in
  let message_type =
    DescriptorProto.{name = None; nested_type=message_types; enum_type = enum_types;
                     field = []; extension; extension_range = []; oneof_decl = [];
                     options = None; reserved_range = []; reserved_name = []; }
  in
  let implementation = Code.init () in
  emit_header implementation ~name ~syntax ~params;
  Code.emit implementation `None "open Ocaml_protoc_plugin.Runtime [@@warning \"-33\"]";
  List.iter ~f:(Code.emit implementation `None "open %s [@@warning \"-33\"]" ) params.opens;
  let _ = match dependencies with
    | [] -> ()
    | dependencies ->
      Code.emit implementation `None "(**/**)";
      Code.emit implementation `Begin "module %s = struct" Scope.import_module_name;
      List.iter ~f:(fun proto_file ->
          let module_name = Scope.module_name_of_proto proto_file in
          Code.emit implementation `None "module %s = %s" module_name module_name;
        ) dependencies;
      Code.emit implementation `End "end";
      Code.emit implementation `None "(**/**)";
  in
  let _signature', implementation' =
    wrap_packages ~params ~syntax ~options scope message_type services (Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package)
  in

  Code.append implementation implementation';
  Code.emit implementation `None "";

  let base_name = Filename.remove_extension name in
  (base_name ^ ".ml"), implementation
