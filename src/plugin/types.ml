open StdLabels
(* Extend scope to have a map over default enum values. enum type string -> enum default value string *)
(* That should be in the type db, and possibly a different map. *)
(* We have the full type name (I assume), so we can take the first in the list, and strip the last . off to get the name. *)
(* We should also have proto name -> proto default name. Then we can lookup, as we already do *)


(* This module is a bit elaborate.
   The idea is to construct the actual types needed
   in the spec module.

   This will ensure that the plugin will only construct valid types,
   so that changes to the spec will require changes here also.
*)

module T = Ocaml_protoc_plugin.Spec.Make(struct
    type ('a, 'deser, 'ser) dir = (string * string * string * string option * (string * string) list)
  end)
open T

open Spec.Descriptor.Google.Protobuf


type type_modifier =
  | No_modifier of string (* The default value *)
  | Optional
  | List
  | Required
  | Oneof_type of string * (string * string) list

type type' =
  { name: string; modifier: type_modifier }

type c = {
  name : string;
  type' : type';
  serialize_spec: string;
  deserialize_spec: string;
}

type field_spec = {
  typestr : string;
  serialize_spec: string;
  deserialize_spec: string;
}

type t = {
  type' : string;
  constructor: string;
  apply: string;
  deserialize_spec: string;
  serialize_spec: string;
  default_constructor_sig: string;
  default_constructor_impl: string;
  merge_impl: string;
}

let sprintf = Printf.sprintf

let make_default: type a. a spec -> string -> a = function
  | Double -> float_of_string
  | Float -> float_of_string

  | Int32 -> Int32.of_string
  | UInt32 -> Int32.of_string
  | SInt32 -> Int32.of_string
  | Fixed32 -> Int32.of_string
  | SFixed32 -> Int32.of_string

  | Int32_int -> int_of_string
  | UInt32_int -> int_of_string
  | SInt32_int -> int_of_string
  | Fixed32_int -> int_of_string
  | SFixed32_int -> int_of_string

  | UInt64 -> Int64.of_string
  | Int64 -> Int64.of_string
  | SInt64 -> Int64.of_string
  | Fixed64 -> Int64.of_string
  | SFixed64 -> Int64.of_string

  | UInt64_int -> int_of_string
  | Int64_int -> int_of_string
  | SInt64_int -> int_of_string
  | Fixed64_int -> int_of_string
  | SFixed64_int -> int_of_string

  | Bool -> bool_of_string
  | String -> fun x -> x
  | Bytes -> Bytes.of_string
  | Enum _ -> fun x -> failwith (sprintf "Defaults for enums cannot be handled here: %s" x) (* Scope.get_scoped_name ~postfix:x scope type_name*)
  | Message _ -> failwith "Messages do not have defaults"

let string_of_default: type a. a spec -> a -> string = function
  | Double -> string_of_float
  | Float -> string_of_float

  | Int32 -> sprintf "%ldl"
  | UInt32 -> sprintf "%ldl"
  | SInt32 -> sprintf "%ldl"
  | Fixed32 -> sprintf "%ldl"
  | SFixed32 -> sprintf "%ldl"

  | Int32_int -> string_of_int
  | UInt32_int -> string_of_int
  | SInt32_int -> string_of_int
  | Fixed32_int -> string_of_int
  | SFixed32_int -> string_of_int

  | Int64 -> sprintf "%LdL"
  | UInt64 -> sprintf "%LdL"
  | SInt64 -> sprintf "%LdL"
  | Fixed64 -> sprintf "%LdL"
  | SFixed64 -> sprintf "%LdL"

  | UInt64_int -> string_of_int
  | Int64_int -> string_of_int
  | SInt64_int -> string_of_int
  | Fixed64_int -> string_of_int
  | SFixed64_int -> string_of_int

  | Bool -> string_of_bool
  | String -> sprintf "{|%s|}"
  | Bytes -> fun bytes -> sprintf "(Bytes.of_string {|%s|})" (Bytes.to_string bytes)
  | Enum (_, _, _,  Some s, _) -> fun _ -> s
  | Enum (s', s, _,  None, _) -> fun _ -> sprintf "(%s 0 (* And its an %s *))" s' s (* Is this the ocaml name???. Maybe we need the protoc name  *)
  | Message _ -> failwith "Messages defaults are not relevant"

let default_of_spec: type a. a spec -> a = fun spec -> match spec with
  | Double -> 0.0
  | Float -> 0.0

  | Int32 -> 0l
  | UInt32 -> 0l
  | SInt32 -> 0l
  | Fixed32 -> 0l
  | SFixed32 -> 0l

  | Int32_int -> 0
  | UInt32_int -> 0
  | SInt32_int -> 0
  | Fixed32_int -> 0
  | SFixed32_int -> 0

  | Int64 -> 0L
  | UInt64 -> 0L
  | SInt64 -> 0L
  | Fixed64 -> 0L
  | SFixed64 -> 0L

  | UInt64_int -> 0
  | Int64_int -> 0
  | SInt64_int -> 0
  | Fixed64_int -> 0
  | SFixed64_int -> 0

  | Bool -> false
  | String -> ""
  | Bytes -> Bytes.of_string ""
  | Enum _-> failwith "Enums not handled here"
  | Message _ -> failwith "Messages defaults are not relevant"

let string_of_spec: type a. [`Deserialize | `Serialize] -> a spec -> string = fun dir spec ->
  match dir, spec with
  | _, Double -> "double"
  | _, Float -> "float"

  | _, Int32 -> "int32"
  | _, UInt32 -> "uint32"
  | _, SInt32 -> "sint32"
  | _, Fixed32 -> "fixed32"
  | _, SFixed32 -> "sfixed32"

  | _, Int32_int -> "int32_int"
  | _, UInt32_int -> "uint32_int"
  | _, SInt32_int -> "sint32_int"
  | _, Fixed32_int -> "fixed32_int"
  | _, SFixed32_int -> "sfixed32_int"

  | _, UInt64 -> "uint64"
  | _, Int64 -> "int64"
  | _, SInt64 -> "sint64"
  | _, Fixed64 -> "fixed64"
  | _, SFixed64 -> "sfixed64"

  | _, UInt64_int -> "uint64_int"
  | _, Int64_int -> "int64_int"
  | _, SInt64_int -> "sint64_int"
  | _, Fixed64_int -> "fixed64_int"
  | _, SFixed64_int -> "sfixed64_int"

  | _, Bool -> "bool"
  | _, String -> "string"
  | _, Bytes -> "bytes"
  | `Deserialize, Enum (_, deser, _ , _, _)  -> sprintf "(enum %s)" deser
  | `Serialize,   Enum (_, _,    ser, _, _)  -> sprintf "(enum %s)" ser
  | `Deserialize, Message (_, deser, _ , _, _) -> sprintf "(message %s)" deser
  | `Serialize,   Message (_, _,    ser, _, _) -> sprintf "(message %s)" ser

let type_of_spec: type a. a spec -> string = function
  | Double -> "float"
  | Float -> "float"

  | Int32 -> "int32"
  | UInt32 -> "int32"
  | SInt32 -> "int32"
  | Fixed32 -> "int32"
  | SFixed32 -> "int32"

  | Int32_int -> "int"
  | UInt32_int -> "int"
  | SInt32_int -> "int"
  | Fixed32_int -> "int"
  | SFixed32_int -> "int"

  | UInt64 -> "int64"
  | Int64 -> "int64"
  | SInt64 -> "int64"
  | Fixed64 -> "int64"
  | SFixed64 -> "int64"

  | UInt64_int -> "int"
  | Int64_int -> "int"
  | SInt64_int -> "int"
  | Fixed64_int -> "int"
  | SFixed64_int -> "int"

  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "bytes"
  | Enum (type', _, _, _, _) -> type'
  | Message (type', _, _, _, _) -> type'

let spec_of_message ~scope type_name =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let deserialize_func =
    let from_proto = Scope.get_scoped_name ~postfix:"from_proto_exn" scope type_name in
    let merge = Scope.get_scoped_name ~postfix:"merge" scope type_name in
    sprintf "((fun writer -> %s writer), %s)" from_proto merge
  in
  let serialize_func = Scope.get_scoped_name ~postfix:"to_proto'" scope type_name in
  Message (type', deserialize_func, serialize_func, None, [])

let spec_of_enum ~scope type_name default =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let deserialize_func = Scope.get_scoped_name ~postfix:"from_int_exn" scope type_name in
  let serialize_func = Scope.get_scoped_name ~postfix:"to_int" scope type_name in
  let default =
    match default with
    | Some default ->
      Option.value_exn type_name
      |> (fun type_name -> sprintf "%s.%s" type_name default)
      |> Option.some
      |> Scope.get_scoped_name scope
    | None ->
      Scope.get_scoped_enum_name scope type_name
  in
  (type', deserialize_func, serialize_func, Some default, [])

open Parameters
let spec_of_type ~params ~scope type_name default =
  let open FieldDescriptorProto.Type in
  function
  | TYPE_DOUBLE   -> Espec Double
  | TYPE_FLOAT    -> Espec Float

  | TYPE_INT64  when params.int64_as_int -> Espec Int64_int
  | TYPE_UINT64 when params.int64_as_int -> Espec UInt64_int
  | TYPE_SINT64 when params.int64_as_int -> Espec SInt64_int

  | TYPE_UINT32 when params.int32_as_int -> Espec UInt32_int
  | TYPE_INT32  when params.int32_as_int -> Espec Int32_int
  | TYPE_SINT32 when params.int32_as_int -> Espec SInt32_int

  | TYPE_FIXED32  when params.fixed_as_int -> Espec Fixed32_int
  | TYPE_SFIXED32 when params.fixed_as_int -> Espec SFixed32_int
  | TYPE_FIXED64  when params.fixed_as_int -> Espec Fixed64_int
  | TYPE_SFIXED64 when params.fixed_as_int -> Espec SFixed64_int

  | TYPE_INT64  -> Espec Int64
  | TYPE_UINT64 -> Espec UInt64
  | TYPE_SINT64 -> Espec SInt64

  | TYPE_UINT32 -> Espec UInt32
  | TYPE_INT32  -> Espec Int32
  | TYPE_SINT32 -> Espec SInt32

  | TYPE_FIXED32  -> Espec Fixed32
  | TYPE_SFIXED32 -> Espec SFixed32
  | TYPE_FIXED64  -> Espec Fixed64
  | TYPE_SFIXED64 -> Espec SFixed64

  | TYPE_BOOL     -> Espec Bool
  | TYPE_STRING   -> Espec String
  | TYPE_BYTES    -> Espec Bytes

  | TYPE_GROUP    -> failwith "Groups not supported"
  | TYPE_MESSAGE  -> Espec (spec_of_message ~scope type_name)
  | TYPE_ENUM     -> Espec (Enum (spec_of_enum ~scope type_name default))

let string_of_oneof_elem dir (Oneof_elem (index, spec, (_, deser, ser, _, _))) =
  let spec_string = string_of_spec dir spec in
  let s = match dir with `Deserialize -> deser | `Serialize -> ser in
  sprintf "oneof_elem (%d, %s, %s)" index spec_string s

let string_of_proto_type: type a. a spec -> a -> string = fun spec default ->
  sprintf "(%s)" (string_of_default spec default)

let string_of_packed = function
  | Packed -> "packed"
  | Not_packed -> "not_packed"

let string_of_type = function
  | { name; modifier = (No_modifier _ | Required | Oneof_type _); _ } -> name
  | { name; modifier = List; _ } -> sprintf "%s list" name
  | { name; modifier = Optional; _ } -> sprintf "%s option" name

let c_of_compound: type a. string -> a compound -> c = fun name -> function
  | Basic (index, spec, default) ->
    let deserialize_spec = sprintf "basic (%d, %s, %s)" index (string_of_spec `Deserialize spec) (string_of_proto_type spec default) in
    let serialize_spec = sprintf "basic (%d, %s, %s)" index (string_of_spec `Serialize spec) (string_of_proto_type spec default) in
    let modifier =
      match spec with
      | Message _ -> Optional
      | _ -> No_modifier (string_of_default spec default)
    in
    let type' = { name = type_of_spec spec; modifier } in
    { name; type'; deserialize_spec; serialize_spec }
  | Basic_req (index, spec) ->
    let deserialize_spec = sprintf "basic_req (%d, %s)" index (string_of_spec `Deserialize spec) in
    let serialize_spec = sprintf "basic_req (%d, %s)" index (string_of_spec `Serialize spec) in
    let type' = { name = type_of_spec spec; modifier = Required } in
    { name; type'; deserialize_spec; serialize_spec }
  | Basic_opt (index, spec) ->
    let deserialize_spec = sprintf "basic_opt (%d, %s)" index (string_of_spec `Deserialize spec) in
    let serialize_spec = sprintf "basic_opt (%d, %s)" index (string_of_spec `Serialize spec) in
    let type' = { name = type_of_spec spec; modifier = Optional } in
    { name; type'; deserialize_spec; serialize_spec }
  | Repeated (index, spec, packed) ->
    let deserialize_spec = sprintf "repeated (%d, %s, %s)" index (string_of_spec `Deserialize spec) (string_of_packed packed) in
    let serialize_spec = sprintf "repeated (%d, %s, %s)" index (string_of_spec `Serialize spec) (string_of_packed packed) in
    let type' = { name = type_of_spec spec; modifier = List } in
    { name; type'; deserialize_spec; serialize_spec; }
  | Oneof (type', deserialize_spec, serialize_spec, _, fields) ->
    let deserialize_spec = sprintf "oneof (%s)" deserialize_spec in
    let serialize_spec = sprintf "oneof (%s)" serialize_spec in

    let type' = { name = type'; modifier = Oneof_type ({|`not_set|}, fields) } in
    { name; type'; deserialize_spec; serialize_spec }

let c_of_field ~params ~syntax ~scope field =
  let open FieldDescriptorProto in
  let open FieldDescriptorProto.Type in
  let number = Option.value_exn field.number in
  let name = Option.value_exn field.name in
  match syntax, field with
  (* This function cannot handle oneof types *)
  | _, { oneof_index = Some _; proto3_optional = Some false | None; _ } -> failwith "Cannot handle oneofs"
  (* Optional messages cannot have a default *)
  | _, { type' = Some TYPE_MESSAGE; default_value = Some _; _ } ->
    failwith "Message types cannot have a default value"
  (* Proto3 cannot have defaults *)
  | `Proto3, { default_value = Some _; _ } ->
    failwith "Default values illegal under proto3"
  (* Proto3 does not support required fields *)
  | `Proto3, { label = Some Label.LABEL_REQUIRED; _ } ->
    failwith "Required fields illegal under proto3"

  (* Optional message *)
  | _, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Basic_opt (number, spec)
    |> c_of_compound name

  (* Required message *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Basic_req (number, spec)
    |> c_of_compound name

  (* Enum under proto2 with a default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = Some default; _ } ->
    let spec = spec_of_enum ~scope type_name (Some default) in
    Basic (number, Enum spec, default)
    |> c_of_compound name

  (* Enum under proto2 with no default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = None; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic_opt (number, Enum spec)
    |> c_of_compound name

  (* Required Enum under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some TYPE_ENUM; type_name; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic_req (number, Enum spec)
    |> c_of_compound name

  (* Required fields under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some type'; type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_req (number, spec)
    |> c_of_compound name

  (* Proto2 optional fields with a default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = Some default; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name (Some default) type' in
    let default = make_default spec default in
    Basic (number, spec, default)
    |> c_of_compound name

  (* Proto2 optional fields - no default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = None; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_opt (number, spec)
    |> c_of_compound name

  (* Proto3 explicitly optional field are mapped as proto2 optional fields *)
  | _, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; proto3_optional = Some true; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_opt (number, spec)
    |> c_of_compound name

  (* Proto3 enum implicitly optional field *)
  | `Proto3, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; _} ->
    let spec = spec_of_enum ~scope type_name None in
    let (_, _, _, default, _) = spec in
    Basic (number, Enum spec, default)
    |> c_of_compound name

  (* Proto3 implicitly optional field *)
  | `Proto3, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; _} ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    let default = default_of_spec spec in
    Basic (number, spec, default)
    |> c_of_compound name

  (* Repeated fields cannot have a default *)
  | _, { label = Some Label.LABEL_REPEATED; default_value = Some _; _ } -> failwith "Repeated fields does not support default values"

  (* Repeated message *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
    Repeated (number, spec, Not_packed)
    |> c_of_compound name

  (* Repeated bytes and strings are not packed *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some (TYPE_STRING | TYPE_BYTES as type'); type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Repeated (number, spec, Not_packed)
    |> c_of_compound name

  (* Repeated enum *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_ENUM; type_name; options; _} ->
    let spec = spec_of_enum ~scope type_name None in
    let packed = match syntax, options with
      | _, Some FieldOptions.{ packed = Some true; _ } -> Packed
      | _, Some FieldOptions.{ packed = Some false; _ } -> Not_packed
      | `Proto2, _ -> Not_packed
      | `Proto3, _ -> Packed
    in
    Repeated (number, Enum spec, packed)
    |> c_of_compound name

  (* Repeated basic type *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some type'; type_name; options; _} ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    let packed = match syntax, options with
      | _, Some FieldOptions.{ packed = Some true; _ } -> Packed
      | _, Some FieldOptions.{ packed = Some false; _ } -> Not_packed
      | `Proto2, _ -> Not_packed
      | `Proto3, _ -> Packed
    in
    Repeated (number, spec, packed)
    |> c_of_compound name
  | _, { label = None; _ } -> failwith "Label not set on field struct"
  | _, { type' = None; _ } -> failwith "Type must be set"


let spec_of_field ~params ~syntax ~scope field : field_spec =
  let c = c_of_field ~params ~syntax ~scope field in
  {
    typestr = string_of_type c.type';
    serialize_spec = c.serialize_spec;
    deserialize_spec = c.deserialize_spec;
  }

let c_of_oneof ~params ~syntax:_ ~scope OneofDescriptorProto.{ name; _ } fields =
  let open FieldDescriptorProto in
  (* Construct the type. *)
  let field_infos =
    List.map ~f:(function
        | { number = Some number; name; type' = Some type'; type_name; _ } ->
          let Espec spec = spec_of_type ~params ~scope type_name None type' in
          (number, name, type_of_spec spec, Espec spec)
        | _ -> failwith "No index or type"
      ) fields
  in
  let oneof =
    let oneof_elems =
      field_infos
      |>
      List.map ~f:(fun (index, name, type', Espec spec) ->
        let adt_name = Scope.get_name_exn scope name in
        adt_name, Oneof_elem (index, spec, (type', sprintf "fun v -> %s v" adt_name, "v", None, []))
      )
    in
    let type' =
      field_infos
      |> List.map ~f:(fun (_, name, type', _) -> sprintf "%s of %s" (Scope.get_name_exn scope name) type')
      |> String.concat ~sep:" | "
      |> sprintf "[ `not_set | %s ]"
    in
    let deser_oneofs =
      oneof_elems
      |> List.map ~f:snd
      |> List.map ~f:(string_of_oneof_elem `Deserialize)
      |> String.concat ~sep:"; "
      |> sprintf "[ %s ]"
    in
    let ser_oneof =
      "| `not_set -> failwith \"This case should never _ever_ happen\"" ::
      List.map oneof_elems ~f:(fun (name, oneof_elem) ->
        sprintf "%s v -> %s" name (string_of_oneof_elem `Serialize oneof_elem)
      )
      |> String.concat ~sep:" | "
      |> sprintf "(function %s)"
    in
    let constructors =
      List.map oneof_elems ~f:(fun (name, Oneof_elem (_, spec, _)) ->
        name, string_of_spec `Deserialize spec
      )
    in
    Oneof (type', deser_oneofs, ser_oneof, None, constructors)
  in

  c_of_compound (Option.value_exn name) oneof


(** Return a list of plain fields + a list of fields per oneof_decl *)
let split_oneof_decl fields oneof_decls =
  let open FieldDescriptorProto in
  let rec filter_oneofs acc rest index = function
    | { oneof_index = Some i; _ } as f :: fs when i = index ->
      filter_oneofs (f :: acc) rest index fs
    | f :: fs -> filter_oneofs acc (f :: rest) index fs
    | [] -> List.rev acc, List.rev rest
  in
  let rec inner = function
    | { oneof_index = Some i; _ } as f :: fs ->
      let oneofs, fs = filter_oneofs [f] [] i fs in
      let decl = List.nth oneof_decls i in
      `Oneof (decl, oneofs) :: inner fs
    | f :: fs ->
      `Field f :: inner fs
    | [] -> []
  in
  inner fields

let sort_fields fields =
  let number = function
    | FieldDescriptorProto.{ number = Some number; _ } -> number
    | _ -> failwith "All Fields must have a number"
  in
  List.sort ~cmp:(fun v v' -> Int.compare (number v) (number v')) fields

let make ~params ~syntax ~is_cyclic ~is_map_entry ~extension_ranges ~scope ~fields oneof_decls =
  let fields = sort_fields fields in
  let ts =
    split_oneof_decl fields oneof_decls
    |> List.map ~f:(function
      (* proto3 Oneof fields with only one field is mapped as regular field *)
      | `Oneof (_, [ FieldDescriptorProto.{ proto3_optional = Some true; _ } as field] )
      | `Field field -> c_of_field ~params ~syntax ~scope field
      | `Oneof (decl, fields) -> c_of_oneof ~params ~syntax ~scope decl fields
    )
  in
  let has_extensions = match extension_ranges with [] -> false | _ -> true in

  let constructor_sig_arg = function
    | { name; type' = { name = type_name; modifier = Required }; _ } ->
      sprintf "%s:%s" (Scope.get_name scope name) type_name
    | { name; type' = { name = type_name; modifier = List }; _} ->
      sprintf "?%s:%s list" (Scope.get_name scope name) type_name
    | { name; type' = { name = type_name; modifier = (Optional | No_modifier _ | Oneof_type _) }; _} ->
      sprintf "?%s:%s" (Scope.get_name scope name) type_name
  in
  let constructor_arg c =
    let name = Scope.get_name scope c.name in
    match c with
    | { type' = { modifier = Required; _}; _ } -> sprintf "~%s" name
    | { type' = { modifier = Optional; _ }; _} -> sprintf "?%s" name
    | { type' = { modifier = List; _ }; _} -> sprintf "?(%s = [])" name
    | { type' = { modifier = (No_modifier default | Oneof_type (default, _)); _}; _} -> sprintf "?(%s = %s)" name default
  in
  let prepend ?(cond=true) elm l = match cond with
    | true -> elm :: l
    | false -> l
  in
  let append ?(cond=true) elm l = match cond with
    | true -> l @ [elm]
    | false -> l
  in

  let t_as_tuple = is_map_entry ||
                   (List.length ts = 1 &&
                    params.singleton_record = false &&
                    not has_extensions &&
                    not is_cyclic)
  in
  let type_constr fields = match fields, t_as_tuple with
    | [], _ -> "unit"
    | fields, true ->
      String.concat ~sep:" * " fields
      |> sprintf "(%s)"
    | fields, false ->
      String.concat ~sep:"; " fields
      |> sprintf "{ %s }"
  in
  let type_destr fields = match fields, t_as_tuple with
    | [], _ -> "()"
    | fields, true ->
      String.concat ~sep:", " fields
      |> sprintf "(%s)"
    | fields, false ->
      String.concat ~sep:"; " fields
      |> sprintf "{ %s }"
  in

  let type' =
    List.rev_map ~f:(fun { name; type'; _} -> ((Scope.get_name scope name), (string_of_type type'))) ts
    |> prepend ~cond:has_extensions ("extensions'", "Runtime'.Extensions.t")
    |> List.rev_map ~f:(function
      | (_, type') when t_as_tuple -> type'
      | (name, type') -> sprintf "%s: %s" name type'
    )
    |> type_constr
  in
  let field_names =
    List.map ~f:(fun { name; _} -> Scope.get_name scope name) ts
    |> append ~cond:has_extensions "extensions'"
  in

  let args = String.concat ~sep:" " field_names in
  let constructor =
    match field_names with
    | [] -> (type_destr field_names)
    | _ ->
      sprintf "fun %s -> %s"
        args (type_destr field_names)
  in
  let apply = match t_as_tuple && not is_map_entry with
    | true -> "serialize"
    | false ->
      sprintf "fun writer %s -> serialize writer %s"
      (type_destr field_names) args
  in

  let default_constructor_sig =
    List.rev_map ~f:constructor_sig_arg ts
    |> prepend ~cond:has_extensions "?extensions':Runtime'.Extensions.t"
    |> prepend "unit"
    |> prepend "t"
    |> List.rev
    |> String.concat ~sep:" -> "
  in
  let default_constructor_impl =
    let args =
      List.rev_map ~f:constructor_arg ts
      |> prepend ~cond:has_extensions "?(extensions' = Runtime'.Extensions.default)"
      |> prepend "()"
      |> List.rev
      |> String.concat ~sep: " "
    in

    let constructor =
      List.rev_map ~f:(fun {name; _} -> sprintf "%s" (Scope.get_name scope name)) ts
      |> prepend ~cond:has_extensions "extensions'"
      |> List.rev
      |> type_destr
    in
    sprintf "%s = %s" args constructor
  in
  let nil =
    match has_extensions with
    | true ->
      extension_ranges
      |> List.map ~f:(fun (start', end') -> sprintf "(%d, %d)" start' end')
      |> String.concat ~sep:"; "
      |> sprintf "nil_ext [ %s ]"
    | false -> "nil"
  in
  (* Create the deserialize spec *)
  let deserialize_spec =
    let spec = List.map ~f:(fun (c : c) -> c.deserialize_spec) ts in
    String.concat ~sep:" ^:: " (spec @ [nil])
    |> sprintf "Runtime'.Deserialize.C.( %s )"
  in

  let serialize_spec =
    let spec = List.map ~f:(fun (c : c) -> c.serialize_spec) ts in
    String.concat ~sep:" ^:: " (spec @ [nil])
    |> sprintf "Runtime'.Serialize.C.( %s )"
  in

  let merge_impl =
    let as_tuple = t_as_tuple || List.length ts = 0 && not has_extensions in
    let args =
      List.map ["t1";"t2"] ~f:(fun s ->
        match as_tuple with
        | true ->
          List.map ~f:(fun c -> sprintf "%s_%s" s (Scope.get_name scope c.name)) ts
          |> String.concat ~sep:","
          |> sprintf "(%s)"
        | false -> s
      )
      |> String.concat ~sep:" "
    in
    let sep = match as_tuple with true -> "_" | false -> "." in
    let merge_values =
      List.map ts ~f:(function
        | { name; deserialize_spec = _; type' = { modifier = Oneof_type (_, ctrs); _ }; _ } ->
          (* Default values for oneof fields makes absolutely no sense!.
             Consider a oneof type with two fields with a default value.
             Its undecidable if any should be marked as set if none of the fields
             are transmitted. The system should actually warn (or error) if
             a syntax2 oneof field is marked with a default value
          *)

          let name = Scope.get_name scope name in
          sprintf "match ((t1%s%s), (t2%s%s)) with"sep name sep name ::
          List.map ~f:(fun (ctr, type') ->
            let spec = sprintf "basic_req (0, %s)" type' in (* Oneof messages are marked as required, as one must be set. *)
            sprintf "  | (%s v1, %s v2) -> %s (Runtime'.Merge.merge Runtime'.Deserialize.C.( %s ) v1 v2)" ctr ctr ctr spec
          ) ctrs
          |> append "  | (v1, `not_set)  -> v1"
          |> append "  | (_, v2) -> v2"
          |> String.concat ~sep:"\n"
          |> fun value -> name, value

        | { name; deserialize_spec; _ } ->
          let name = Scope.get_name scope name in
          name, sprintf "Runtime'.Merge.merge Runtime'.Deserialize.C.( %s ) t1%s%s t2%s%s"
            deserialize_spec sep name sep name
      )
      |> append ~cond:has_extensions ("extensions'", sprintf "List.append t1%sextensions' t2%sextensions'" sep sep)
    in
    let constr =
      match as_tuple with
      | true ->
        List.map ~f:snd merge_values
        |> String.concat ~sep:","
        |> sprintf "(%s)"
      | false ->
        List.map merge_values ~f:(fun (name, value) ->
          Printf.sprintf "%s = (%s);" name value
        )
        |> String.concat ~sep:"\n"
        |> sprintf "{\n%s\n }"
    in
    sprintf "fun %s -> %s" args constr
  in

  (* The type contains optional elements. We should not have those *)
  { type'; constructor; apply; deserialize_spec; serialize_spec; default_constructor_sig; default_constructor_impl; merge_impl }
