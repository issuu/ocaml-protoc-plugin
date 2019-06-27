open Core

(* TODO: Make all module recursive. This is a bit tricky, as we need to construct the signature of the module,
   but for now we dont care - But we do need to handle it!*)

(* All fields will have a value - or be set to a default value.
   This means that only fields referencing other messages will be optional.
   - Even enum fields will have a default value.

   We need to track the path for all types, as they seem to be fully qualified.
   Relative types are prefixed with '.'

   Service functions could be implemented as simple as:
   module Service = sig
     val name: string
     type request = ...
     type response = ...
     val encode_request: request -> string
     val decode_request: string -> request result
     val encode_response: response -> string
     val decode_response: string -> response result
   end

   User code could then implement something like:
   val create_service: (module F : Service_type) -> ~handler:(F.request -> F.response Deferred.Result.t) -> string -> string Deferred.Result.t
*)

(* Remember to mangle reserved keywords *)
let type_name name =
  String.uncapitalize (Option.value_exn name)

(* Remember to mangle reserved keywords *)
let module_name name =
  String.capitalize (Option.value_exn name)

(* Remember to mangle reserved keywords *)
let field_name name =
  String.uncapitalize (Option.value_exn name)

let constructor_name { Spec.Descriptor.name; number = _; options = _} =
  String.capitalize (Option.value_exn name)


let to_string_opt = function
  | Some s -> s
  | None -> "<None>"

(** Slightly overloaded name here. Its also used for all other types which would go into a module *)
type message = { module_name: string;
                 signature: Code.t;
                 implementation: Code.t;
               }


let log fmt = eprintf (fmt ^^ "\n%!")

let emit_enum_type Spec.Descriptor.{ name;
                                     value;
                                     options = _;
                                     reserved_range = _;
                                     reserved_name = _ } : message =
  let module_name = module_name name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let t = Code.init () in
  Code.emit t `None "type t = %s"
    (List.map ~f:constructor_name value |> String.concat ~sep:" | ");
  Code.append signature t;
  Code.append implementation t;
  { module_name; signature; implementation }

(* Message type should have a name a signature and an implementation. These should then be declared recursivly.
   But at this point its not possible to join them.

   Also inner modules needs to be declared in the outer module. So inner signatures needs to be returned to the outer.
   If we create a code peice for the inner signature, it should be ok, as it will contain all inner signatures recursivly.

   The implementation should also be carried to the outer level in the same manner.

   Service descriptions should be a functor over the io monad.

*)
let emit_field t scope Spec.Descriptor.{ name;
                                         number = _;
                                         label = _;
                                         type_;
                                         type_name = t_name;
                                         extendee = _; (* Extensions are not supported *)
                                         default_value = _;
                                         oneof_index = _;
                                         json_name = _;
                                         options = _;
                                       } =
  let type_spec =
    match Option.value_exn type_ with
    | Spec.Descriptor.Type_double
    | Spec.Descriptor.Type_float -> "float"
    | Spec.Descriptor.Type_int64
    | Spec.Descriptor.Type_uint64
    | Spec.Descriptor.Type_int32
    | Spec.Descriptor.Type_fixed64
    | Spec.Descriptor.Type_fixed32
    | Spec.Descriptor.Type_sfixed32
    | Spec.Descriptor.Type_sfixed64
    | Spec.Descriptor.Type_sint32
    | Spec.Descriptor.Type_sint64
    | Spec.Descriptor.Type_uint32 -> "int"
    | Spec.Descriptor.Type_bool -> "bool"
    | Spec.Descriptor.Type_string -> "string"
    | Spec.Descriptor.Type_group -> failwith "Deprecated"
    | Spec.Descriptor.Type_message ->  Scope.get_scoped_name scope t_name ^ " option"
    | Spec.Descriptor.Type_bytes -> "bytes"
    | Spec.Descriptor.Type_enum -> Scope.get_scoped_name scope t_name
  in
  Code.emit t `None "%s: %s;" (field_name name) (type_spec)

(* This should return: (module name, sig, impl) *)
let rec emit_message_type scope Spec.Descriptor.{ name;
                                                  field = fields;
                                                  extension = _;
                                                  nested_type = nested_types;
                                                  enum_type = enum_types;
                                                  extension_range = _;
                                                  oneof_decl = _;
                                                  options = _;
                                                  reserved_range = _;
                                                  reserved_name = _;
                                                } : message =

  let rec emit_nested_types ~signature ~implementation ?(is_first=true) nested_types =
    let emit_sub dest ~is_implementation ~is_first { module_name; signature; implementation } =
      let () = match is_first with
        | true ->
          Code.emit dest `Begin "module rec %s : sig" module_name;
        | false ->
          Code.emit dest `Begin "and %s : sig" module_name;
      in
      Code.append dest signature;
      let () = match is_implementation with
        | false -> ()
        | true ->
          Code.emit dest `EndBegin "end = struct ";
          Code.append dest implementation
      in
      Code.emit dest `End "end";
      ()
    in
    match nested_types with
    | [] -> ()
    | sub :: subs ->
      emit_sub ~is_implementation:false signature ~is_first sub;
      emit_sub ~is_implementation:true implementation ~is_first sub;
      emit_nested_types ~signature ~implementation ~is_first:false subs
  in

  let signature = Code.init () in
  let implementation = Code.init () in
  let module_name = module_name name in
  let scope = Scope.push scope module_name in
  List.map ~f:emit_enum_type enum_types @ List.map ~f:(emit_message_type scope) nested_types
  |> emit_nested_types ~signature ~implementation;

  let t = Code.init () in
  let () = match fields with
    | [] -> ()
    | fields ->
      Code.emit t `Begin "type t = {";
      List.iter ~f:(emit_field t scope) fields;
      Code.emit t `End  "}"
  in

  Code.append signature t;
  Code.append implementation t;
  { module_name; signature; implementation }



let parse_proto_file { Spec.Descriptor.name;
                       package = package;
                       dependency = _;
                       public_dependency = _;
                       weak_dependency = _;
                       message_type = message_types;
                       enum_type = enum_types;
                       service = _;
                       extension = _;
                       options = _;
                       source_code_info = _;
                       syntax;
                     } =
  log "parse_proto_file: Name = %s. Package=%s, syntax=%s. enums: %d"
    (to_string_opt name)
    (to_string_opt package)
    (to_string_opt syntax)
    (List.length enum_types)
  ;

  (* Artificially create messages to emulate package scope *)
  let message_type =
    Option.value_map ~default:[] ~f:(String.split ~on:'.') package
    |> (fun l -> "" :: l)
    |> List.rev
    |> List.fold_left
         ~init:(message_types, enum_types)
         ~f:(fun (nested_types, enum_types) name ->
           eprintf "Enclose in module: %s (%d)\n" name (List.length nested_types);
           let message_type = Spec.Descriptor.default_descriptor_proto ~name:(Some name) ~nested_type:nested_types ~enum_type:enum_types () in
           ([message_type], [])
         )
    |> function ([message_type], []) -> message_type
              | _ -> failwith "Invariant broken"
  in
  let scope = Scope.init () in
  let { module_name = _; signature = _; implementation } = emit_message_type scope message_type in
  implementation

let parse_request { Spec.Plugin.file_to_generate; parameter; proto_file; compiler_version=_ } =
  log "Request to parse proto_files: %s. Parameter: %s"
    (String.concat ~sep:"; " file_to_generate)
    (Option.value ~default:"<None>" parameter);

  List.map ~f:parse_proto_file proto_file
  |> List.iter ~f:(Code.dump)
