open StdLabels
open Parameters
open Spec.Descriptor.Google.Protobuf

module IntSet = Set.Make(struct type t = int let compare = compare end)
let sprintf = Printf.sprintf
let to_string_opt = function
  | Some s -> s
  | None -> "<None>"

(** Slightly overloaded name here.
    Its also used for all other types which would go into a module *)
type module' = {
  module_name : string;
  signature : Code.t;
  implementation : Code.t;
}

let emit_enum_type ~scope ~params
    EnumDescriptorProto.{name; value; options = _; reserved_range = _; reserved_name = _}
    : module'
  =
  let module_name = Scope.get_message_name scope name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let t = Code.init () in
  Code.emit t `None "type t = %s %s"
    (List.map ~f:(fun f -> Names.module_name (Option.value_exn f.EnumValueDescriptorProto.name)) value
     |> String.concat ~sep:" | ") params.Parameters.annot;
  Code.append signature t;
  Code.append implementation t;
  Code.emit signature `None "val to_int: t -> int";
  Code.emit signature `None "val from_int: int -> (t, [> Runtime'.Result.error]) result";
  Code.emit implementation `Begin "let to_int = function";
  List.iter ~f:(fun v ->
      let open  EnumValueDescriptorProto in
      Code.emit implementation `None "| %s -> %d" (Names.module_name (Option.value_exn v.name)) (Option.value_exn v.number)
    ) value;
  Code.emit implementation `End "";
  Code.emit implementation `Begin "let from_int = function";

  let _ =
    List.fold_left ~init:IntSet.empty ~f:(fun seen v ->
        let idx = (Option.value_exn v.EnumValueDescriptorProto.number) in
        match IntSet.mem idx seen with
        | true -> seen
        | false ->
          let open EnumValueDescriptorProto in
          Code.emit implementation `None "| %d -> Ok %s" idx (Names.module_name (Option.value_exn v.name));
          IntSet.add idx seen
      ) value
  in
  Code.emit implementation `None "| n -> Error (`Unknown_enum_value n)";
  Code.emit implementation `End "";
  {module_name; signature; implementation}

(* Argh. Service types should also be mapped *)
let emit_service_type scope ServiceDescriptorProto.{ name; method' = methods; _ } =
  let emit_method t MethodDescriptorProto.{ name; input_type; output_type; _} =
    Code.emit t `Begin "let %s = " (Names.field_name (Option.value_exn name));
    Code.emit t `None "( (module %s : Runtime'.Service.Message with type t = %s ), "
      (Scope.get_scoped_name scope input_type)
      (Scope.get_scoped_name ~postfix:"t" scope input_type);
    Code.emit t `End "  (module %s : Runtime'.Service.Message with type t = %s ) ) "
      (Scope.get_scoped_name scope output_type)
      (Scope.get_scoped_name ~postfix:"t" scope output_type)
  in
  let t = Code.init () in
  Code.emit t `Begin "module %s = struct" (Names.module_name (Option.value_exn ~message:"Service entries must have a name" name));
  List.iter ~f:(emit_method t) methods;
  Code.emit t `End "end";
  t

let emit_extension ~scope ~params field =
  let FieldDescriptorProto.{ name; extendee; _ } = field
  in
  let module_name = (Names.module_name (Option.value_exn ~message:"Extensions must have a name" name)) in
  let extendee_type = Scope.get_scoped_name scope ~postfix:"t" extendee in
  let extendee_field = Scope.get_scoped_name scope ~postfix:"extensions'" extendee in
  (* Create the type of the type' / type_name *)
  let t =
    let params = Parameters.{params with singleton_record = false} in
    Types.make ~params ~syntax:`Proto2 ~is_cyclic:false ~scope ~is_map_entry:false ~has_extensions:false ~fields:[field] []
  in

  let signature = Code.init () in
  let implementation = Code.init () in
  Code.append implementation signature;

  Code.emit signature `None "type t = %s %s" t.type' params.annot;
  Code.emit signature `None "val get: %s -> (%s, [> Runtime'.Result.error]) result" extendee_type t.type';
  Code.emit signature `None "val set: %s -> %s -> %s" extendee_type t.type' extendee_type;

  Code.emit implementation `None "type t = %s %s" t.type' params.annot;
  Code.emit implementation `None "let get extendee = Runtime'.Extensions.get %s (extendee.%s) |> Runtime'.Result.open_error" t.deserialize_spec extendee_field ;
  Code.emit implementation `Begin "let set extendee t =";
  Code.emit implementation `None "let extensions' = Runtime'.Extensions.set (%s) (extendee.%s) t in" t.serialize_spec extendee_field;
  Code.emit implementation `None "{ extendee with %s = extensions' }" extendee_field;
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

  let has_extensions = not (extension_ranges = []) in
  (* Ignore empty modules *)
  let module_name, scope =
    match name with
    | None -> "", scope
    | Some name ->
      let module_name = Scope.get_message_name scope (Some name) in
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
      let extension_ranges =
        extension_ranges
        |> List.map ~f:(function
            | DescriptorProto.ExtensionRange.{ start = Some start; end' = Some end'; _ } -> (start, end')
            | _ -> failwith "Extension ranges must be defined"
          )
        |> List.map ~f:(fun (s, e) -> sprintf "(%d, %d)" s e)
        |> String.concat ~sep:"; "
        |> sprintf "[%s]"
      in
      Code.emit signature `None "val name': unit -> string";
      Code.emit implementation `None "let name' () = \"%s\"" (Scope.get_current_scope scope);
      let Types.{ type'; constructor; apply; deserialize_spec; serialize_spec } =
        Types.make ~params ~syntax ~is_cyclic ~is_map_entry ~has_extensions ~scope ~fields oneof_decls
      in
      Code.emit signature `None "type t = %s %s" type' params.annot;
      Code.emit signature `None "val to_proto: t -> Runtime'.Writer.t";
      Code.emit signature `None "val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result";

      Code.emit implementation `None "type t = %s%s" type' params.annot;

      Code.emit implementation `Begin "let to_proto =";
      Code.emit implementation `None "let apply = %s in" apply;
      Code.emit implementation `None "let spec = %s in" serialize_spec;
      Code.emit implementation `None "let serialize = Runtime'.Serialize.serialize %s (spec) in" extension_ranges;
      Code.emit implementation `None "fun t -> apply ~f:serialize t";
      Code.emit implementation `End "";

      Code.emit implementation `Begin "let from_proto =";
      Code.emit implementation `None "let constructor = %s in" constructor;
      Code.emit implementation `None "let spec = %s in" deserialize_spec;
      Code.emit implementation `None "let deserialize = Runtime'.Deserialize.deserialize %s spec constructor in" extension_ranges;
      Code.emit implementation `None "fun writer -> deserialize writer |> Runtime'.Result.open_error";
      Code.emit implementation `End "";
    | None -> ()
  in
  {module_name; signature; implementation}

let rec wrap_packages ~params ~syntax scope message_type services = function
  | [] ->
    let {module_name = _; implementation; _} = emit_message ~params ~syntax scope message_type in
    List.iter ~f:(fun service ->
        Code.append implementation (emit_service_type scope service)
      ) services;
    implementation

  | package :: packages ->
    let implementation = Code.init () in
    let package_name = Names.module_name package in (* This should be based on mangle_names *)
    let scope = Scope.push scope package in
    Code.emit implementation `Begin "module %s = struct" package_name;
    Code.append implementation (wrap_packages ~params ~syntax scope message_type services packages);
    Code.emit implementation `End "end";
    implementation

let parse_proto_file ~params scope
    FileDescriptorProto.{ name; package; dependency = _; public_dependency = _;
                          weak_dependency = _; message_type = message_types;
                          enum_type = enum_types; service = services; extension;
                          options = _; source_code_info = _; syntax; }
  =
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

  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(*       AUTOGENERATED FILE - DO NOT EDIT!      *)";
  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(* Generated by: ocaml-protoc-plugin            *)";
  Code.emit implementation `None "(* https://github.com/issuu/ocaml-protoc-plugin *)";
  Code.emit implementation `None "(************************************************)";
  Code.emit implementation `None "(*";
  Code.emit implementation `None "   Source: %s" (to_string_opt name);
  Code.emit implementation `None "   Syntax: %s " (match syntax with `Proto2 -> "proto2" | `Proto3 -> "proto3");
  Code.emit implementation `None "   Parameters:";
  Code.emit implementation `None "     debug=%b" params.debug;
  Code.emit implementation `None "     annot='%s'" params.annot;
  Code.emit implementation `None "     opens=[%s]" (String.concat ~sep:"; " params.opens);
  Code.emit implementation `None "     int64_as_int=%b" params.int64_as_int;
  Code.emit implementation `None "     int32_as_int=%b" params.int32_as_int;
  Code.emit implementation `None "     fixed_as_int=%b" params.fixed_as_int;
  Code.emit implementation `None "     singleton_record=%b" params.singleton_record;
  Code.emit implementation `None "*)";
  Code.emit implementation `None "";
  Code.emit implementation `None "open Ocaml_protoc_plugin.Runtime";
  List.iter ~f:(Code.emit implementation `None "open %s") params.opens;

  wrap_packages ~params ~syntax scope message_type services (Option.value_map ~default:[] ~f:(String.split_on_char ~sep:'.') package)
  |> Code.append implementation;


  let out_name =
    Option.map ~f:(fun proto_file_name ->
        Filename.remove_extension proto_file_name
        |> sprintf "%s.ml"
      ) name
  in
  out_name, implementation
