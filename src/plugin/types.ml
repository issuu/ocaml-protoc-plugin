open StdLabels

(* This module is a bit elaborate.
   The idea is to construct the actual types needed
   in the spec module.

   This will ensure that the plugin will only construct valid types,
   so that chnges to the spec will require changes here also.
*)

module T = Ocaml_protoc_plugin.Spec.Make(struct
    type ('a, 'deser, 'ser) dir = (string * string * string * string option)
  end)
open T


(* Existential types *)
type espec = Espec: _ spec -> espec
type c = {
  name : string;
  type' : string;
  serialize_spec: string;
  deserialize_spec: string;
}

type t = {
  type' : string;
  constructor: string;
  apply: string;
  deserialize_spec: string;
  serialize_spec: string;
}

open Spec.Descriptor.Google.Protobuf

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

  | Int32 -> Int32.to_string
  | UInt32 -> Int32.to_string
  | SInt32 -> Int32.to_string
  | Fixed32 -> Int32.to_string
  | SFixed32 -> Int32.to_string

  | Int32_int -> string_of_int
  | UInt32_int -> string_of_int
  | SInt32_int -> string_of_int
  | Fixed32_int -> string_of_int
  | SFixed32_int -> string_of_int

  | UInt64 -> Int64.to_string
  | Int64 -> Int64.to_string
  | SInt64 -> Int64.to_string
  | Fixed64 -> Int64.to_string
  | SFixed64 -> Int64.to_string

  | UInt64_int -> string_of_int
  | Int64_int -> string_of_int
  | SInt64_int -> string_of_int
  | Fixed64_int -> string_of_int
  | SFixed64_int -> string_of_int

  | Bool -> string_of_bool
  | String -> sprintf "{|%s|}"
  | Bytes -> fun bytes -> sprintf "(Bytes.of_string {|%s|})" (Bytes.to_string bytes)
  | Enum (_, _, _,  s) -> fun _ -> Option.value_exn s
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
  | `Deserialize, Enum (_, deser, _ , _)  -> sprintf "(enum %s)" deser
  | `Serialize,   Enum (_, _,    ser, _)  -> sprintf "(enum %s)" ser
  | `Deserialize, Message (_, deser, _ , _) -> sprintf "(message %s)" deser
  | `Serialize,   Message (_, _,    ser, _) -> sprintf "(message %s)" ser

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
  | Enum (type', _, _, _) -> type'
  | Message (type', _, _, _) -> type'

let spec_of_message ~scope type_name =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let deserialize_func = Scope.get_scoped_name ~postfix:"from_proto" scope type_name in
  let serialize_func = Scope.get_scoped_name ~postfix:"to_proto" scope type_name in
  Message (type', deserialize_func, serialize_func, None)

let spec_of_enum ~scope type_name default =
  let type' = Scope.get_scoped_name ~postfix:"t" scope type_name in
  let deserialize_func = Scope.get_scoped_name ~postfix:"from_int" scope type_name in
  let serialize_func = Scope.get_scoped_name ~postfix:"to_int" scope type_name in
  let default = Option.map ~f:(fun default -> Scope.get_scoped_name ~postfix:default scope type_name) default in
  Enum (type', deserialize_func, serialize_func, default)

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
  | TYPE_ENUM     -> Espec (spec_of_enum ~scope type_name default)

let string_of_oneof_elem dir (Oneof_elem (index, spec, (_, deser, ser, _))) =
  let spec_string = string_of_spec dir spec in
  let s = match dir with `Deserialize -> deser | `Serialize -> ser in
  sprintf "oneof_elem (%d, %s, %s)" index spec_string s

let string_of_proto_type: type a. a spec -> a proto_type -> string = fun spec -> function
  | Proto3 -> "proto3"
  | Proto2 None -> "proto2 (none)"
  | Proto2 (Some default) -> sprintf "proto2 (some (%s))" (string_of_default spec default)
  | Required -> "required"

let string_of_packed = function
  | Packed -> "packed"
  | Not_packed -> "not_packed"

let c_of_compound: type a. string -> a compound -> c = fun name ->
  let name = Names.field_name (Some name) in
  function

  | Basic (index, spec, proto_type) ->
    let deserialize_spec = sprintf "basic (%d, %s, %s)" index (string_of_spec `Deserialize spec) (string_of_proto_type spec proto_type) in
    let serialize_spec = sprintf "basic (%d, %s, %s)" index (string_of_spec `Serialize spec) (string_of_proto_type spec proto_type) in
    let type' = type_of_spec spec in
    { name; type'; deserialize_spec; serialize_spec }
  | Basic_opt (index, spec) ->
    let deserialize_spec = sprintf "basic_opt (%d, %s)" index (string_of_spec `Deserialize spec) in
    let serialize_spec = sprintf "basic_opt (%d, %s)" index (string_of_spec `Serialize spec) in
    let type' = sprintf "%s option" (type_of_spec spec) in
    { name; type'; deserialize_spec; serialize_spec }
  | Repeated (index, spec, packed) ->
    let type' = sprintf "%s list" (type_of_spec spec) in
    let deserialize_spec = sprintf "repeated (%d, %s, %s)" index (string_of_spec `Deserialize spec) (string_of_packed packed) in
    let serialize_spec = sprintf "repeated (%d, %s, %s)" index (string_of_spec `Serialize spec) (string_of_packed packed) in
    { name; type'; deserialize_spec; serialize_spec }
  | Oneof (type', deserialize_spec, serialize_spec, _) ->
    let deserialize_spec = sprintf "oneof (%s)" deserialize_spec in
    let serialize_spec = sprintf "oneof (%s)" serialize_spec in
    { name; type'; deserialize_spec; serialize_spec }

let c_of_field ~params ~syntax ~scope field =
  let open FieldDescriptorProto in
  let open FieldDescriptorProto.Type in
  let number = Option.value_exn field.number in
  let name = Option.value_exn field.name in
  match syntax, field with
  (* This function cannot handle oneof types *)
  | _, { oneof_index = Some _; _ } -> failwith "Cannot handle oneofs"
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
    Basic (number, spec, Required)
    |> c_of_compound name

  (* Enum under proto2 with a default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = Some default; _ } ->
    (*
    let default =
      Scope.get_scoped_name ~postfix:default scope type_name
      (* |> Printf.sprintf "(*S*) proto2 (some %s) (*E*)" *)
    in
*)
    let spec = spec_of_enum ~scope type_name (Some default) in
    Basic (number, spec, Proto2 (Some default))
    |> c_of_compound name

  (* Enum under proto2 with no default value *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some TYPE_ENUM; type_name; default_value = None; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic_opt (number, spec)
    |> c_of_compound name

  (* Required Enum under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some TYPE_ENUM; type_name; _ } ->
    let spec = spec_of_enum ~scope type_name None in
    Basic (number, spec, Required)
    |> c_of_compound name

  (* Required fields under proto2 *)
  | `Proto2, { label = Some Label.LABEL_REQUIRED; type' = Some type'; type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic (number, spec, Required)
    |> c_of_compound name

  (* Proto2 optional fields with a default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = Some default; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name (Some default) type' in
    let default = make_default spec default in
    Basic (number, spec, Proto2 (Some default))
    |> c_of_compound name

  (* Proto2 optional fields - no default *)
  | `Proto2, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; default_value = None; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic_opt (number, spec)
    |> c_of_compound name

  (* Proto3 optional fields *)
  | `Proto3, { label = Some Label.LABEL_OPTIONAL; type' = Some type'; type_name; _ } ->
    let Espec spec = spec_of_type ~params ~scope type_name None type' in
    Basic (number, spec, Proto3)
    |> c_of_compound name

  (* Repeated fields cannot have a default *)
  | _, { label = Some Label.LABEL_REPEATED; default_value = Some _; _ } -> failwith "Repeated fields does not support default values"

  (* Repeated message *)
  | _, { label = Some Label.LABEL_REPEATED; type' = Some Type.TYPE_MESSAGE; type_name; _ } ->
    let spec = spec_of_message ~scope type_name in
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
    Repeated (number, spec, packed)
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

let c_of_oneof ~params ~syntax:_ ~scope OneofDescriptorProto.{ name; _ } fields =
  let open FieldDescriptorProto in
  (* Construct the type. *)
  let field_infos =
    List.map ~f:(function
        | { number = Some number; name; type' = Some type'; type_name; _ } ->
          let Espec spec = spec_of_type ~params ~scope type_name None type' in
          (number, name, type_of_spec spec, (Espec spec))
        | _ -> failwith "No index or type"
      ) fields
  in
  let type' =
    field_infos
    |> List.map ~f:(fun (_, name, type', _) -> sprintf "`%s of %s" (Names.constructor_name name) type')
    |> String.concat ~sep:" | "
    |> sprintf "[ %s ]"
  in

  (* Create deserialization: *)
  let deser_oneofs =
    field_infos
    |> List.map ~f:(fun (index, name, type', Espec spec) ->
        Oneof_elem (index, spec, (type', sprintf "fun v -> `%s v" (Names.constructor_name name), "<not used>", None))
      )
    |> List.map ~f:(string_of_oneof_elem `Deserialize)
    |> String.concat ~sep:"; "
    |> sprintf "[ %s ]"
  in
  let ser_oneof =
    field_infos
    |> List.map ~f:(fun (index, name, type', Espec spec) ->
        let oneof_elem = Oneof_elem (index, spec, (type', "<not used>", "v", None)) in
        sprintf "`%s v -> %s" (Names.constructor_name name) (string_of_oneof_elem `Serialize oneof_elem)
      )
    |> String.concat ~sep:" | "
    |> sprintf "(function %s)"
  in
  let oneof = Oneof (type', deser_oneofs, ser_oneof, None) in

  c_of_compound (Names.field_name name) oneof


(** Return a list of plain fields + a list of fields per oneof_decl *)
let split_oneof_decl fields oneof_decls =
  let open FieldDescriptorProto in
  let rec inner oneofs oneof_decls = function
    | { oneof_index = Some i; _ } as o1 :: fs -> begin
        match oneofs with
        | [] -> inner [o1] oneof_decls fs
        | { oneof_index = Some j; _ } :: _ when i = j ->
          inner (o1 :: oneofs) oneof_decls fs
        | oneofs ->
          `Oneof (List.hd oneof_decls, List.rev oneofs) :: inner [o1] (List.tl oneof_decls) fs
      end
    | f :: fs -> begin
        match oneofs with
        | [] -> `Field f :: inner [] oneof_decls fs
        | oneofs ->
          `Oneof (List.hd oneof_decls, List.rev oneofs) :: `Field f :: inner [] (List.tl oneof_decls) fs
      end
    | [] -> begin
        match oneofs, oneof_decls with
        | [], [] -> []
        | oneofs, [oneof_decl] ->
          [ `Oneof (oneof_decl, List.rev oneofs) ]
        | _ -> failwith "No field or no oneof"
      end
  in
  inner [] oneof_decls fields

(* Let create everything *)
let make ~params ~syntax ~is_map_entry ~scope ~fields oneof_decls =
  let ts =
    split_oneof_decl fields oneof_decls
    |> List.map ~f:(function
        | `Field field -> c_of_field ~params ~syntax ~scope field
        | `Oneof (decl, fields) -> c_of_oneof ~params ~syntax ~scope decl fields
      )
  in
  let (type', constructor, apply) =
    match ts with
    | [] -> "unit", "()", "fun ~f () -> f"
    | [ { type'; _ } ] when params.singleton_record = false ->
      type', "fun a -> a", "fun ~f a -> f a"
    | [_; _] when is_map_entry ->
      let type' =
        List.map ~f:(fun { name = _; type'; _} -> sprintf "%s" type') ts
        |> String.concat ~sep:" * "
        |> sprintf "(%s)"
      in
      type', "fun a b -> (a, b)", "fun ~f (a, b) -> f a b"
    | ts ->
      let type' =
        List.map ~f:(fun { name; type'; _} -> sprintf "%s: %s" name type') ts
        |> String.concat ~sep:"; "
        |> sprintf "{ %s }"
      in
      let constructor, apply =
        let field_names = List.map ~f:(fun { name; _} -> name) ts in
        let args = String.concat ~sep:" " field_names in
        let constr = String.concat ~sep:"; " field_names in
        let constructor = sprintf "fun %s -> { %s }" args constr in
        let apply = sprintf "fun ~f:f' { %s } -> f' %s" constr args in
        constructor, apply
      in
      (type', constructor, apply)
  in
  (* Create the deserialize spec *)
  let deserialize_spec =
    let spec = List.map ~f:(fun (c:c) -> c.deserialize_spec) ts in
    String.concat ~sep:" ^:: " (spec @ ["nil"])
    |> sprintf "Ocaml_protoc_plugin.Deserialize.C.( %s )"
  in

  let serialize_spec =
    let spec = List.map ~f:(fun (c:c) -> c.serialize_spec) ts in
    String.concat ~sep:" ^:: " (spec @ ["nil"])
    |> sprintf "Ocaml_protoc_plugin.Serialize.C.( %s )"
  in
  { type'; constructor; apply; deserialize_spec; serialize_spec }
