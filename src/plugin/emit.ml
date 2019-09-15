open Core_kernel

(* This should be part of options sent to proc so its under user control *)
let annot = "[@@deriving show { with_path = false }]"

(* TODO: Package should be embeded in the constructed file.
   - Another way is to make a outer module with the filename again
*)

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
     repeated field3: int = 3;
     repeated field4: X = 4;
     oneof ofield {
       field5 int32 = 5;
       field6 int32 = 6;
     }
   }

   let rec to_proto: t -> string = fun { field1; field2; field3; field4; ofield } ->
     let field1 = 1, Runtime.serialize_int field1 in
     let field2 = 2, Runtime.serialize_message to_proto field2 in
     let field3 = 3, Runtime.serialize_list Runtime.serialize_int field3 in
     let field4 = 4, Runtime.serialize_list (Runtime.serialize_message to_proto) field4 in
     let ofield =
       match ofield with
       | `Field5 v -> 5, Runtime.serialize_int32 v
       | `Field6 v -> 6, Runtime.serialize_int32 v
     in
     Runtime.serialize_fields [field1; field2; field3; field4; ofield]

   -- To deserialize a oneof, we need to pass the id of the message to the deserialization function.

   let rec of_proto: string -> t = fun message ->
     let field1, get_field1 = Runtime.deserialize_int () in
     let field2, get_field2 = Runtime.deserialize_message of_proto
     let field3, get_field3 = Runtime.deserialize_list Runtime.deserialize_int in
     let field4, get_field4 = Runtime.deserialize_list (Rumtime.deserialize_message of_proto) in
     match Runtime.deserialize_message_spec [1, field1; 2, field2; 3, field3; 4, field4] with
     | Error e -> Error e
     | Ok () -> { field1 = get_field1 (); field2 = get_field2 (); field3 = get_field3 (); field4 = get_field4 }

   The types:
      Runtime.deserialize_int: unit -> (field -> unit result.t) * (unit -> int)
      Runtime.deserialize_message: (string -> 'a) -> (field -> unit result.t) * (unit -> 'a option)
      Runtime.deserialize_list: (field -> unit result.t) * (unit -> 'a) -> (field -> unit result) * (unit -> 'a list)

*)

(** Taken from: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let is_reserved = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done"
  | "downto" | "else" | "end" | "exception" | "external" | "false" | "for" | "fun"
  | "function" | "functor" | "if" | "in" | "include" | "inherit" | "initializer"
  | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor" | "match" | "method"
  | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or"
  | "private" | "rec" | "sig" | "struct" | "then" | "to" | "true" | "try" | "type"
  | "val" | "virtual" | "when" | "while" | "with" ->
    true
  | "from_proto" | "to_proto" -> true
  | _ -> false

(* Remember to mangle reserved keywords *)
let module_name name = String.capitalize (Option.value_exn name)

(* Remember to mangle reserved keywords *)
let field_name (field_name : string option) =
  match String.uncapitalize (Option.value_exn field_name) with
  | name when is_reserved name -> name ^ "'"
  | name -> name

let variant_name name = module_name name

let constructor_name Spec.Descriptor.{name; number = _; options = _} =
  String.capitalize (Option.value_exn name)

let to_string_opt = function
  | Some s -> s
  | None -> "<None>"

(** Slightly overloaded name here.
    Its also used for all other types which would go into a module *)
type message = {
  module_name : string;
  signature : Code.t;
  implementation : Code.t;
}

let log fmt = eprintf (fmt ^^ "\n%!")

let emit_enum_type
    Spec.Descriptor.{name; value; options = _; reserved_range = _; reserved_name = _}
    : message
  =
  let module_name = module_name name in
  let signature = Code.init () in
  let implementation = Code.init () in
  let t = Code.init () in
  Code.emit
    t
    `None
    "type t = %s %s"
    (List.map ~f:constructor_name value |> String.concat ~sep:" | ")
    annot;
  Code.append signature t;
  Code.append implementation t;
  Code.emit signature `None "val to_int: t -> int";
  Code.emit signature `None "val from_int: int -> t Protobuf.Deserialize.result";

  Code.emit implementation `Begin "let to_int = function";
  List.iter ~f:(fun v -> Code.emit implementation `None "| %s -> %d" (constructor_name v) (Option.value_exn v.number)) value;
  Code.emit implementation `End "";

  Code.emit implementation `Begin "let from_int = function";
  List.iter ~f:(fun v ->
      Code.emit implementation `None "| %d -> Ok %s" (Option.value_exn v.number) (constructor_name v)
    ) value;
  Code.emit implementation `None "| n -> Error (`Unknown_enum_value n)";

  Code.emit implementation `End "";




  {module_name; signature; implementation}

(* Message type should have a name a signature and an implementation. These should then be declared recursivly.
   But at this point its not possible to join them.

   Also inner modules needs to be declared in the outer module. So inner signatures needs to be returned to the outer.
   If we create a code peice for the inner signature, it should be ok, as it will contain all inner signatures recursivly.

   The implementation should also be carried to the outer level in the same manner.

   Service descriptions should be a functor over the io monad.

*)

let protobuf_type_of_field ~prefix scope field_descriptor =
  let open Spec.Descriptor in
  let base_type =
    match field_descriptor with
    | {type_ = Some Type_double; _} -> "Double"
    | {type_ = Some Type_float; _} -> "Float"
    | {type_ = Some Type_int64; _} -> "Int64"
    | {type_ = Some Type_uint64; _} -> "UInt64"
    | {type_ = Some Type_int32; _} -> "Int32"
    | {type_ = Some Type_fixed64; _} -> "Fixed64"
    | {type_ = Some Type_fixed32; _} -> "Fixed32"
    | {type_ = Some Type_sfixed32; _} -> "SFixed32"
    | {type_ = Some Type_sfixed64; _} -> "SFixed64"
    | {type_ = Some Type_sint32; _} -> "SInt32"
    | {type_ = Some Type_sint64; _} -> "SInt64"
    | {type_ = Some Type_uint32; _} -> "UInt32"
    | {type_ = Some Type_bool; _} -> "Bool"
    | {type_ = Some Type_string; _} -> "String"
    | {type_ = Some Type_bytes; _} -> "Bytes"
    | {type_ = Some Type_group; _} -> failwith "Groups are deprecated"
    | {type_ = Some Type_message; type_name; _} ->
      let to_proto_func = Scope.get_scoped_name ~postfix:(prefix ^ "_proto") scope type_name in
      sprintf "Message %s" to_proto_func
    | {type_ = Some Type_enum; type_name; _} ->
      let to_int_func = Scope.get_scoped_name ~postfix:(prefix ^ "_int") scope type_name in
      sprintf "Enum %s" to_int_func
    | {type_ = None; _} -> failwith "Abstract types cannot be"
  in
  match field_descriptor with
  | {label = Some Label_repeated; type_ = Some Type_message; type_name; _ } ->
    let to_proto_func = Scope.get_scoped_name ~postfix:(prefix ^ "_proto") scope type_name in
    sprintf "RepeatedMessage %s" to_proto_func
  | {label = Some Label_repeated; _} -> sprintf "Repeated (%s)" base_type
  | _ -> base_type

(** Get the stringified name of a type.
    Consider moving this to Protocol somewhere. So types are next to each other.
    Currently this needs to be in sync with the Protobuf.Serialize.protobuf_type
*)
let type_of_field scope field_descriptor =
  let open Spec.Descriptor in
  let base_type =
    match field_descriptor with
    | {type_ = Some Type_double; _} | {type_ = Some Type_float; _} -> "float"
    | {type_ = Some Type_int64; _}
    | {type_ = Some Type_uint64; _}
    | {type_ = Some Type_int32; _}
    | {type_ = Some Type_fixed64; _}
    | {type_ = Some Type_fixed32; _}
    | {type_ = Some Type_sfixed32; _}
    | {type_ = Some Type_sfixed64; _}
    | {type_ = Some Type_sint32; _}
    | {type_ = Some Type_sint64; _}
    | {type_ = Some Type_uint32; _} ->
      "int"
    | {type_ = Some Type_bool; _} -> "bool"
    | {type_ = Some Type_string; _} -> "string"
    | {type_ = Some Type_group; _} -> failwith "Groups are deprecated"
    | {type_ = Some Type_message; type_name; oneof_index = Some _; _} ->
      Scope.get_scoped_name scope ~postfix:"t" type_name
    | {type_ = Some Type_message; type_name; oneof_index = None; _} ->
      Scope.get_scoped_name ~postfix:"t" scope type_name
    | {type_ = Some Type_bytes; _} -> "bytes"
    | {type_ = Some Type_enum; type_name; _} ->
      Scope.get_scoped_name ~postfix:"t" scope type_name
    | {type_ = None; _} -> failwith "Abstract types cannot be"
  in
  match field_descriptor with
  | {label = Some Label_repeated; _} -> base_type ^ " list"
  | {oneof_index = None; type_ = Some Type_message; _} -> base_type ^ " option"
  | _ -> base_type

let emit_field t scope (field : Spec.Descriptor.field_descriptor_proto) =
  Code.emit t `None "%s: %s;" (field_name field.name) (type_of_field scope field)

let emit_oneof_fields t scope
    ((oneof_decl : Spec.Descriptor.oneof_descriptor_proto), fields)
  =
  (* Emit a polymorphic variant type *)
  let variants =
    List.map
      ~f:(fun field ->
        let type_ = type_of_field scope field in
        let name = variant_name field.name in
        sprintf "`%s of %s" name type_)
      fields
  in
  Code.emit
    t
    `None
    "%s: [ %s ] option;"
    (field_name oneof_decl.name)
    (String.concat ~sep:" | " variants)

(** Return a list of plain fields + a list of fields per oneof_decl *)
let split_oneof_decl fields oneof_decls =
  List.foldi
    ~init:(fields, [])
    ~f:(fun i (fields, oneof_decls) oneof_decl ->
      let oneof_fields, rest =
        List.partition_tf
          ~f:(function
            | {Spec.Descriptor.oneof_index = Some i'; _} -> i = i'
            | {Spec.Descriptor.oneof_index = None; _} -> false)
          fields
      in
      rest, (oneof_decl, oneof_fields) :: oneof_decls)
    oneof_decls

let inject (signature', implementation') signature implementation =
  Code.append signature signature';
  Code.append implementation implementation'

let emit_deserialization_function scope all_fields oneof_decls =
  let fields, _oneof_decls = split_oneof_decl all_fields oneof_decls in

  let signature = Code.init () in
  let implementation = Code.init () in
  Code.emit signature `None "val from_proto: Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) result";

  let _field_names = List.map ~f:(fun field -> field_name field.name) fields in
  (* We should call Deserialize with something *)
  Code.emit implementation `Begin "let from_proto data =";
  Code.emit implementation `None "let open Base.Result.Monad_infix in";
  List.iter ~f:(fun field ->
      let index = Option.value_exn field.number in
      let typ = protobuf_type_of_field ~prefix:"from" scope field in (* Not correct *)
      Code.emit implementation `None "let (sentinal_%d, deser_%d) = Protobuf.Deserialize.sentinal (%s) in" index index typ;
    ) fields;

  let spec =
    List.map ~f:(fun field -> sprintf "(%d, deser_%d)" (Option.value_exn field.number) (Option.value_exn field.number)) fields
    |> String.concat ~sep:"; "
  in
  Code.emit implementation `None "let spec = [ %s ] in" spec;
  Code.emit implementation `None "Protobuf.Deserialize.deserialize spec data >>= fun () -> ";
  (* Construct the record *)
  let construct =
    match List.is_empty fields with
    | true -> "()"
    | false ->
      List.map ~f:(fun field ->
          let name = field_name field.name in
          let index = Option.value_exn field.number in
          sprintf "%s = sentinal_%d ()" name index
        ) fields
      |> String.concat ~sep:"; "
      |> sprintf "{ %s }"
  in

  Code.emit implementation `None "Base.Result.return %s" construct;
  Code.emit implementation `End "";

  signature, implementation

(* Return code for signature and implementation *)
let emit_serialization_function scope all_fields oneof_decls =
  let fields, _oneof_decls = split_oneof_decl all_fields oneof_decls in
  let signature = Code.init () in
  let implementation = Code.init () in
  Code.emit signature `None "val to_proto: t -> Protobuf.Writer.t";
  (* Create a list of protobuf_types *)
  (* to_proto should destruct the type and pass to the function.  *)
  let protocol_field_spec =
    List.map ~f:(fun field -> field.number, protobuf_type_of_field ~prefix:"to" scope field) fields
    |> List.map ~f:(fun (index, tpe) ->
           sprintf "(%d, %s) ^:: " (Option.value_exn index) tpe)
    |> String.concat
  in
  (* Destruct the type. *)
  let field_names = List.map ~f:(fun field -> field_name field.name) fields in
  let destruct =
    match fields with
    | [] -> "()"
    | _ -> String.concat ~sep:"; " field_names |> sprintf "{ %s }"
  in
  Code.emit implementation `Begin "let to_proto %s = " destruct;
  Code.emit implementation `None "let open Protobuf.Serialize in";
  Code.emit
    implementation
    `None
    "serialize (%sNil) %s"
    protocol_field_spec
    (String.concat ~sep:" " field_names);

  Code.emit implementation `End "";

  signature, implementation

let emit_message_type scope all_fields oneof_decls =
  let fields, oneof_decls = split_oneof_decl all_fields oneof_decls in
  let t = Code.init () in
  let () =
    match all_fields with
    | [] -> Code.emit t `None "type t = () %s" annot
    | _ ->
      Code.emit t `Begin "type t = {";
      List.iter ~f:(emit_field t scope) fields;
      List.iter ~f:(emit_oneof_fields t scope) oneof_decls;
      Code.emit t `End "} %s" annot
  in
  t

(*
  (* Lets emit code for the implementaton *)
  let destruct = match fields with
    | [] -> "()"
    | _ ->
      List.map ~f:field_name fields
      |> String.concat ~sep:"; "
      |> sprintf "{ %s }"
  in
  Code.emit t `Begin "let rec to_proto %s = " destruct;
  let emit_serialize_field (field : Spec.Descriptor.field_descriptor_proto) =
    let name = field_name field in
    (* Oneof just lists on all the fields, but will use the same reference *)
    (* So what do we want.

       We have id -> function.
       Function is repeated of type
       Function is one of type
       Function is int -> (int -> enum)
       Function is oneof -> id -> (oneof type)? (* This needs composability *)
    *)


    (* What if its oneof? Then we needs to make a special deserialization function I gather *)
    let serialize_fun = match Option.value_exn field.type_ with
      | Spec.Descriptor.Type_double -> "serialize_double"
      | Type_float -> "serialize_float"
      | Type_uint64 -> "serialize_uint64"
      | Type_int32 -> "serialize_int32"
      | Type_fixed64 -> "serialize_fixed64"
      | Type_fixed32 -> "serialize_fixed32"
      | Type_sfixed32 -> "serialize_sfixed32"
      | Type_sfixed64 -> "serialize_sfixed64"
      | Type_sint32 -> "serialize_sint32"
      | Type_sint64 -> "serialize_sint64"
      | Type_uint32 -> "serialize_uint32" -> "int"
      | Type_bool -> "serialize_bool" -> "bool"
      | Type_string -> "serialize_string" -> "string"
      | Type_group -> failwith "Groups are deprecated"
      | { type_ = Some Type_message; type_name; oneof_index = Some _; _ } ->
      Scope.get_scoped_name scope type_name
    | { type_ = Some Type_message; type_name; oneof_index = None;_ } ->
      Scope.get_scoped_name scope type_name
    | { type_ = Some Type_bytes; _ } -> "bytes"
    | { type_ = Some Type_enum; type_name; _ } ->
      Scope.get_scoped_name scope type_name
    | { type_ = None; _ } -> failwith "Abstract types cannot be"
    match field with
    | { name; number; label; type_; type_name; extendee; default_value;
        oneof_index; json_name; options } -> (??)
  let field1 = Runtime.serialize_int field1 in
  let field2 = Runtime.serialize_message to_proto field2 in
  let field3 = Runtime.serialize_list Runtime.serialize_int field3 in
  let field4 = Runtime.serialize_list (Runtime.serialize_message to_proto) field4 in
  Runtime.serialize_fields [1, field1; 2, field2; 3, field3; 4, field4]


*)

(* This should return: (module name, sig, impl) *)
let rec emit_message scope
    Spec.Descriptor.
      {
        name;
        field = fields;
        extension = _;
        nested_type = nested_types;
        enum_type = enum_types;
        extension_range = _;
        oneof_decl = oneof_decls;
        options = _;
        reserved_range = _;
        reserved_name = _;
      }
    : message
  =
  let rec emit_nested_types ~signature ~implementation ?(is_first = true) nested_types =
    let emit_sub dest ~is_implementation ~is_first
        {module_name; signature; implementation}
      =
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
  let module_name, scope =
    match name with
    | None -> "", scope
    | Some _ ->
      let module_name = module_name name in
      module_name, Scope.push scope module_name
  in
  List.map ~f:emit_enum_type enum_types @ List.map ~f:(emit_message scope) nested_types
  |> emit_nested_types ~signature ~implementation;
  (* Only messages with actual names get a type and serialization functions.
     If the name is None, its because its created by us as placeholder for a package.
  *)
  let () =
    match name with
    | Some _name ->
      Code.emit signature `None "val name: string";
      Code.emit
        implementation
        `None
        "let name = \"%s\""
        (String.concat ~sep:"." (List.rev scope));
      (* Need fully qualified name, plz *)
      let t = emit_message_type scope fields oneof_decls in
      Code.append signature t;
      Code.append implementation t;
      inject
        (emit_serialization_function scope fields oneof_decls)
        signature
        implementation;
      inject
        (emit_deserialization_function scope fields oneof_decls)
        signature
        implementation
    | None -> ()
  in
  {module_name; signature; implementation}

let rec wrap_packages scope message_type = function
  | [] ->
    let {module_name = _; signature; implementation} = emit_message scope message_type in
    signature, implementation
  | [package] ->
    let signature = Code.init () in
    let implementation = Code.init () in
    let package_name = module_name (Some package) in
    let {module_name = _; signature = signature'; implementation = implementation'} =
      emit_message scope message_type
    in
    Code.emit signature `Begin "module %s : sig" package_name;
    Code.emit implementation `Begin "module %s = struct" package_name;
    inject (signature', implementation') signature implementation;
    Code.emit signature `End "end";
    Code.emit implementation `End "end";
    signature, implementation
  | package :: packages ->
    let signature = Code.init () in
    let implementation = Code.init () in
    let package_name = module_name (Some package) in
    let signature', implementation' =
      wrap_packages (Scope.push scope package_name) message_type packages
    in
    Code.emit signature `Begin "module %s : sig" package_name;
    Code.emit implementation `Begin "module %s = struct" package_name;
    inject (signature', implementation') signature implementation;
    Code.emit signature `End "end";
    Code.emit implementation `End "end";
    signature, implementation

let parse_proto_file
    {
      Spec.Descriptor.name;
      package;
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
    }
  =
  log
    "parse_proto_file: Name = %s. Package=%s, syntax=%s. enums: %d"
    (to_string_opt name)
    (to_string_opt package)
    (to_string_opt syntax)
    (List.length enum_types);
  let message_type =
    Spec.Descriptor.default_descriptor_proto
      ~name:None
      ~nested_type:message_types
      ~enum_type:enum_types
      ()
  in
  let _, implementation =
    wrap_packages
      (Scope.init ())
      message_type
      (Option.value_map ~default:[] ~f:(String.split ~on:'.') package)
  in
  let out_name =
    name
    |> Option.map ~f:(fun proto_file_name ->
           (match String.chop_suffix ~suffix:".proto" proto_file_name with
           | None -> proto_file_name
           | Some stem -> stem)
           |> Printf.sprintf "%s.ml")
  in
  out_name, implementation

let parse_request
    {Spec.Plugin.file_to_generate; parameter; proto_file; compiler_version = _}
  =
  log
    "Request to parse proto_files: %s. Parameter: %s"
    (String.concat ~sep:"; " file_to_generate)
    (Option.value ~default:"<None>" parameter);
  let x = List.map ~f:parse_proto_file proto_file in
  List.iter ~f:(fun (_, code) -> Code.dump code) x;
  x
