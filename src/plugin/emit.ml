open Core

(* TODO: Make all module recursive. This is a bit tricky, as we need to construct the signature of the module,
   but for now we dont care - But we do need to handle it!*)

(* All fields will have a value - or be set to a default value.
   This means that only fields referencing other messages will be optional.
   - Even enum fields will have a default value.

   We need to track the path for all types, as they seem to be fully qualified.
   Relative types are prefixed with '.'
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



let to_string_opt = function
  | Some s -> s
  | None -> "<None>"

(** Imperative construct to hold emitted code. It also holds the current path. *)
module Code = struct
  type t = {
    mutable indent: string;
    mutable code: string list;
    filename : string;
    mutable path : string list;
  }
  let init filename = {
    indent = "";
    code = [];
    filename;
    path = [];
  }

  let push t name =
    t.path <- name :: t.path

  let pop t =
    t.path = List.tl_exn t.path

  let make_reference_name t = function
    | Some name -> begin
        match String.split ~on:'.' name with
        | "" :: xs ->
          let rec inner = function
            | x :: xs, y :: ys when String.equal x y -> inner (xs, ys)
            | xs, _ -> List.map ~f:String.capitalize xs |> String.concat ~sep:"." |> sprintf "%s.t"
          in
          inner (xs, List.rev t.path)
        | _ -> failwith "Expected name to start with a '.'"
      end
    | None -> failwith "Does not contain a name"


  let incr t =
    t.indent <- "  " ^ t.indent

  let decr t =
    t.indent <- String.chop_prefix_exn ~prefix:"  " t.indent

  let emit t indent fmt =
    let emit s =
      match indent with
      | `Begin ->
        t.code <- (t.indent ^ s) :: t.code;
        incr t;
      | `None->
        t.code <- (t.indent ^ s) :: t.code
      | `End ->
        decr t;
        t.code <- (t.indent ^ s) :: t.code
      | `EndBegin ->
        t.code <- (String.chop_prefix_exn ~prefix:"  " t.indent ^ s) :: t.code
    in
    Printf.ksprintf emit fmt

  let dump t =
    eprintf "===============\n";
    eprintf "File: %s\n" t.filename;
    List.iter ~f:(eprintf "%s\n") (List.rev t.code);
    eprintf "===============\n";
    ()
end

let log fmt = eprintf (fmt ^^ "\n%!")

let emit_enum_type t Spec.Descriptor.{ name;
                                       value;
                                       options = _;
                                       reserved_range = _;
                                       reserved_name = _ } =
  let constructor_name { Spec.Descriptor.name; number = _; options = _} =
    String.capitalize (Option.value_exn name)
  in

  Code.emit t `Begin "module %s = struct" (module_name name);
  Code.emit t `None "type t = %s"
    (List.map ~f:constructor_name value |> String.concat ~sep:" | ");
  Code.emit t `End "end";
  ()


let rec emit_message_type t Spec.Descriptor.{ name;
                                              field = fields;
                                              extension = _;
                                              nested_type = nested_types;
                                              enum_type = enum_types;
                                              extension_range = _;
                                              oneof_decl = _;
                                              options = _;
                                              reserved_range = _;
                                              reserved_name = _;
                                            } =

  let emit_field t Spec.Descriptor.{ name;
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
      | Spec.Descriptor.Type_group -> failwith "Unhandled"
      | Spec.Descriptor.Type_message ->  Code.make_reference_name t t_name ^ " option"
      | Spec.Descriptor.Type_bytes -> "bytes"
      | Spec.Descriptor.Type_enum -> Code.make_reference_name t t_name
    in
    Code.emit t `None "%s: %s;" (field_name name) (type_spec)
  in

  Code.emit t `Begin "module rec %s : sig" (module_name name);
  List.iter ~f:(emit_enum_type t) enum_types;
  List.iter ~f:(emit_message_type t) nested_types;
  let () = match fields with
    | [] -> ()
    | fields ->
      Code.emit t `Begin "type t = {";
      List.iter ~f:(emit_field t) fields;
      Code.emit t `End  "}"
  in
  Code.emit t `EndBegin "end = struct";
  List.iter ~f:(emit_enum_type t) enum_types;
  List.iter ~f:(emit_message_type t) nested_types;
  let () = match fields with
    | [] -> ()
    | fields ->
      Code.emit t `Begin "type t = {";
      List.iter ~f:(emit_field t) fields;
      Code.emit t `End  "}"
  in
  Code.emit t `End "end";
  ()


let parse_proto_file t { Spec.Descriptor.name;
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
  List.iter ~f:(emit_enum_type t) enum_types;
  List.iter ~f:(emit_message_type t) message_types;
  ()

let parse_request { Spec.Plugin.file_to_generate; parameter; proto_file; compiler_version=_ } =
  log "Request to parse proto_files: %s. Parameter: %s"
    (String.concat ~sep:"; " file_to_generate)
    (Option.value ~default:"<None>" parameter);

  let t = Code.init "some file" in
  List.iter ~f:(parse_proto_file t) proto_file;
  Code.dump t;
  ()
