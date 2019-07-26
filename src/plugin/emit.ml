open Core_kernel

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


   Oneof types are mapped to polymorphic types:

   message X {
     oneof oneof_field {
       int32 field1 = 1;
     };
     int32 field2 = 2;
   };

   module X = struct
     type t = {
       oneof_field : [ `field1 of int ]
       field2 : int;
     }

   for each message, serialization and deserialization function are created:
   - of_proto : deserialization
   - to_proto : serialization


   serialization function for a structure:
   message X {
     field1: int = 1;
     field2: X = 2;
     repeated field2: int = 3;
   }

   let rec to_proto: t -> string = fun { field1; field2 } ->
     let field1 = Runtime.serialize_int field1 in
     let field2 = Runtime.serialize_message to_proto field2 in
     let field3 = Runtime.serialize_list Runtime.serialize_int field3 in
     Runtime.serialize_fields [1, field1; 2, field2; 3, field3]

   let rec of_proto: string -> t = fun message ->
     let field1, get_field1 = Runtime.deserialize_int () in
     let field2, get_field2 = Runtime.deserialize_message of_proto
     let field3, get_field3 = Runtime.deserialize_list Runtime.deserialize_int in
     match Runtime.deserialize_message_spec [1,field1; 2, field2; 3, field3] with
     | Error e -> Error e
     | Ok () -> { field1 = get_field1 (); field2 = get_field2 (); field3 = get_field3 () }

   The types:
      Runtime.deserialize_int: unit -> (field -> unit result.t) * (unit -> int)
      Runtime.deserialize_message: (string -> 'a) -> (field -> unit result.t) * (unit -> 'a option)
      Runtime.deserialize_list: (field -> unit result.t) * (unit -> 'a) -> (field -> unit result) * (unit -> 'a list)
      (* List of fields??? *)

*)

(** Taken from: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let is_reserved = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do"
  | "done" | "downto" | "else" | "end" | "exception" | "external" | "false"
  | "for" | "fun" | "function" | "functor" | "if" | "in" | "include" | "inherit"
  | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor" | "match"
  | "method" | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open"
  | "or" | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type"
  | "val" | "virtual" | "when" | "while" | "with" -> true
  | "from_proto" | "to_proto" -> true
  | _ -> false

(* Remember to mangle reserved keywords *)
let module_name name =
  String.capitalize (Option.value_exn name)

(* Remember to mangle reserved keywords *)
let field_name name =
  match String.uncapitalize (Option.value_exn name) with
  | name when is_reserved name -> name ^ "'"
  | name -> name

let variant_name name = module_name name

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
let type_of_field scope = function
  | Spec.Descriptor.{ type_ = Some Type_double; _ }
  | Spec.Descriptor.{ type_ = Some Type_float; _ } -> "float"
  | Spec.Descriptor.{ type_ = Some Type_int64; _ }
  | Spec.Descriptor.{ type_ = Some Type_uint64; _ }
  | Spec.Descriptor.{ type_ = Some Type_int32; _ }
  | Spec.Descriptor.{ type_ = Some Type_fixed64; _ }
  | Spec.Descriptor.{ type_ = Some Type_fixed32; _ }
  | Spec.Descriptor.{ type_ = Some Type_sfixed32; _ }
  | Spec.Descriptor.{ type_ = Some Type_sfixed64; _ }
  | Spec.Descriptor.{ type_ = Some Type_sint32; _ }
  | Spec.Descriptor.{ type_ = Some Type_sint64; _ }
  | Spec.Descriptor.{ type_ = Some Type_uint32; _ } -> "int"
  | Spec.Descriptor.{ type_ = Some Type_bool; _ } -> "bool"
  | Spec.Descriptor.{ type_ = Some Type_string; _ } -> "string"
  | Spec.Descriptor.{ type_ = Some Type_group; _ } -> failwith "Deprecated"
  | Spec.Descriptor.{ type_ = Some Type_message; type_name; oneof_index = Some _; _ } -> Scope.get_scoped_name scope type_name
  | Spec.Descriptor.{ type_ = Some Type_message; type_name; oneof_index = None;_ } -> Scope.get_scoped_name scope type_name ^ " option"
  | Spec.Descriptor.{ type_ = Some Type_bytes; _ } -> "bytes"
  | Spec.Descriptor.{ type_ = Some Type_enum; type_name; _ } -> Scope.get_scoped_name scope type_name
  | Spec.Descriptor.{ type_ = None; _ } -> failwith "Abstract types cannot be"

let emit_field t scope (field : Spec.Descriptor.field_descriptor_proto) =
  Code.emit t `None "%s: %s;" (field_name field.name) (type_of_field scope field)

let emit_oneof_fields t scope ( (oneof_decl : Spec.Descriptor.oneof_descriptor_proto), fields ) =
  (* Emit a polymorphic variant type *)
  let variants = List.map ~f:(fun field ->
      let type_ = type_of_field scope field in
      let name = variant_name field.name in
      sprintf "`%s of %s" name type_
    ) fields
  in
  Code.emit t `None "%s: [ %s ] option;" (field_name oneof_decl.name) (String.concat ~sep: " | " variants)

(** Return a list of plain fields + a list of fields per oneof_decl *)
let split_oneof_decl fields oneof_decls =
  List.foldi ~init:(fields, []) ~f:(fun i (fields, oneof_decls) oneof_decl ->
      let (oneof_fields, rest) =
        List.partition_tf ~f:(function
            | { Spec.Descriptor.oneof_index = Some i'; _ } -> i = i'
            | { Spec.Descriptor.oneof_index = None; _ } -> false
          ) fields
      in
      (rest, (oneof_decl, oneof_fields) :: oneof_decls)
    ) oneof_decls

(* This should return: (module name, sig, impl) *)
let rec emit_message_type scope Spec.Descriptor.{ name;
                                                  field = all_fields;
                                                  extension = _;
                                                  nested_type = nested_types;
                                                  enum_type = enum_types;
                                                  extension_range = _;
                                                  oneof_decl = oneof_decls;
                                                  options = _;
                                                  reserved_range = _;
                                                  reserved_name = _;
                                                } : message =


  (* Filter fields which are part of the oneofs *)
  eprintf "%s: Fields: %d - oneofs: %d\n" (module_name name) (List.length all_fields) (List.length oneof_decls);
  let (fields, oneof_decls) = split_oneof_decl all_fields oneof_decls in
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
  (* Ignore empty modules *)
  let module_name, scope = match name with
    | None | Some "" ->
      "", scope
    | Some _ ->
      let module_name = module_name name in
      module_name, Scope.push scope module_name
  in
  eprintf "Current scope in %s: %s\n" module_name (Scope.get_current_scope scope);
  List.map ~f:emit_enum_type enum_types @ List.map ~f:(emit_message_type scope) nested_types
  |> emit_nested_types ~signature ~implementation;

  let t = Code.init () in
  let () = match all_fields with
    | [] -> ()
    | _ ->
      Code.emit t `Begin "type t = {";
      List.iter ~f:(emit_field t scope) fields;
      List.iter ~f:(emit_oneof_fields t scope) oneof_decls;
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
