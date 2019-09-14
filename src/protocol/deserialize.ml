open Core
open Result.Let_syntax

type error =
  [ Protobuffer.error
  | `Wrong_field_type of string * Spec.field
  | `Illegal_value of string * Spec.field
  | `Not_implemented
  | `Unknown_enum_value of int] [@@deriving show]

(** Module for deserializing values *)
type nonrec 'a result = ('a, error) result

type _ spec =
  | Double : float spec
  | Float : float spec
  | Int32 : int spec
  | Int64 : int spec
  | UInt32 : int spec
  | UInt64 : int spec
  | SInt32 : int spec
  | SInt64 : int spec
  | Fixed32 : int spec (* unsigned *)
  | Fixed64 : int spec
  | SFixed32 : int spec
  | SFixed64 : int spec
  | Bool : bool spec
  | String : string spec
  | Bytes : bytes spec
  | Message : (string -> 'a result) -> 'a option spec
  | Enum : (int -> 'a result) -> 'a spec
  | Repeated : 'a spec -> 'a list spec

type 'a sentinal = unit -> 'a

type decoder = Spec.field -> unit result

module Defaults = struct
  let bool = false
  let int = 0
  let float = 0.0
  let string = ""
  let bytes = Bytes.create 0
  let enum = 0
  let message = None
end

let error_wrong_field str field : _ result =
  `Wrong_field_type (str, field) |> Result.fail

let error_illegal_value str field : _ result =
  `Illegal_value (str, field) |> Result.fail


(** Deserialize a buffer. *)
let read_fields : Protobuffer.t -> ((int * Spec.field) list) result = fun t ->
  let rec inner acc =
    match Protobuffer.has_more t with
    | false -> return (List.rev acc) (* Preserve order *)
    | true ->
      let%bind v = Protobuffer.read_field t in
      inner (v :: acc)
  in
  match inner [] with
  | Ok v -> return v
  | Error `Premature_end_of_input -> Result.fail `Premature_end_of_input
  | Error (`Unknown_field_type n) -> Result.fail (`Unknown_field_type n)


(** Deserialize takes a list of field sentinals and a buffer for the
    data. Data is decoded into a list of tags, fields
    and the appropiate function for the tag is called.
    After all fields have been processed,
    data can be read from the sentinals, created by the caller

    A helper function exists to create sentinals
*)
let deserialize : (int * decoder) list -> Protobuffer.t -> unit result = fun spec buffer ->
  (* Exceptions here is an error in code-generation. Crash hard on that! *)
  let decoder_map = Map.of_alist_exn (module Int) spec in
  let%bind fields = read_fields buffer in
  List.fold_left
    ~init:(Result.Ok ())
    ~f:(fun acc (field_idx, field_val) ->
      let%bind () = acc in
      match Map.find decoder_map field_idx with
      | None -> return ()
      | Some f -> f field_val)
    fields

let double_sentinal () =
  let value = ref Defaults.float in
  ( (fun () -> !value),
    function
    | Spec.Fixed_64_bit v ->
      value := Int64.float_of_bits v;
      Result.ok_unit
    | field -> error_wrong_field "double" field )

let float_sentinal () =
  let value = ref Defaults.float in
  ( (fun () -> !value),
    function
    | Spec.Fixed_32_bit v ->
      value := Int32.float_of_bits v;
      Result.ok_unit
    | field -> error_wrong_field "float" field )

let decode_signed_int v = v

let int_sentinal ~signed ~type_name () =
  let value = ref Defaults.int in
  ( (fun () -> !value),
    function
    | Spec.Varint v when signed ->
      (* Need to decode a signed int proper *)
      value := decode_signed_int v;
      Result.ok_unit
    | Spec.Varint v ->
      value := v;
      Result.ok_unit
    | field -> error_wrong_field type_name field )

let varint_sentinal ~signed ~type_name =
  let value = ref Defaults.int in
  ( (fun () -> !value),
    function
    | Spec.Varint v ->
      let v = match signed with
        | true when v mod 2 = 0 ->
          v / 2
        | true  ->
          v / 2 * (-1) - 1
        | false ->
          v
      in
      value := v;
      Result.ok_unit
    | field -> error_wrong_field type_name field )

let uint32_sentinal () = varint_sentinal ~signed:false ~type_name:"uint32"
let sint32_sentinal () = varint_sentinal ~signed:true ~type_name:"sint32"
let int32_sentinal () = varint_sentinal ~signed:false ~type_name:"int32"
let uint64_sentinal () = varint_sentinal ~signed:false ~type_name:"uint64"
let sint64_sentinal () = varint_sentinal ~signed:true ~type_name:"sint64"
let int64_sentinal () = varint_sentinal ~signed:false ~type_name:"int64"

let fixed32_sentinal ~type_name =
  let value = ref Defaults.int in
  ( (fun () -> !value),
    function
    | Spec.Fixed_32_bit v ->
      value := Int32.to_int_exn v;
      return ()
    | field -> error_wrong_field type_name field )

let sfixed32_sentinal () = fixed32_sentinal ~type_name:"sfixed32"

let fixed32_sentinal () = fixed32_sentinal ~type_name:"fixed32"

let fixed64_sentinal ~type_name =
  let value = ref Defaults.int in
  ( (fun () -> !value),
    function
    | Spec.Fixed_64_bit v ->
      value := Int64.to_int_exn v;
      return ()
    | field -> error_wrong_field type_name field )

let sfixed64_sentinal () = fixed64_sentinal ~type_name:"sfixed64"

let fixed64_sentinal () = fixed64_sentinal ~type_name:"fixed64"

let bool_sentinal () =
  let value = ref Defaults.bool in
  ( (fun () -> !value),
    function
    | Spec.Varint v when v = 0 ->
      value := false;
      Result.ok_unit
    | Spec.Varint v when v = 1 ->
      value := true;
      Result.ok_unit
    | Spec.Varint _ as field -> error_illegal_value "bool" field
    | field -> error_wrong_field "bool" field )

let string_sentinal () =
  let value = ref Defaults.string in
  ( (fun () -> !value),
    function
    | Spec.Length_delimited data ->
      value := data;
      Result.ok_unit
    | field -> error_wrong_field "string" field )

let bytes_sentinal () =
  let value = ref Defaults.bytes in
  ( (fun () -> !value),
    function
    | Spec.Length_delimited data ->
      value := Bytes.of_string data;
      Result.ok_unit
    | field -> error_wrong_field "bytes" field )

let message_sentinal deser =
  let value = ref None in
  ( (fun () -> !value),
    function
    | Spec.Length_delimited data ->
      let%bind message = deser data in
      value := Some message;
      return ()
    | field -> error_wrong_field "message" field )

let enum_sentinal deser =
  let value =
    match deser Defaults.enum with
    | Ok v -> ref v
    | Error _ -> failwith "Internal error: Enum _must_ allow default value."
  in
  (fun () -> !value),
  function
  | Spec.Varint v ->
    let%bind v = deser v in
    value := v;
    return ()
  | field -> error_wrong_field "enum" field

(** Repeated field. For scalar types, we allow a length delim, and
    just read all values in it. For this reason, we need to know the exact type.
*)
let rec read_fields ~f buffer acc =
  match Protobuffer.has_more buffer with
  | true ->
    let%bind v = f buffer in
    read_fields ~f buffer (v :: acc)
  | false ->
    return acc

let repeated_sentinal ~scalar_type (get, read) =
  let value = ref [] in
  let read_field v =
    let%bind () = read v in
    value := get () :: !value;
    return ()
  in
  (fun () -> List.rev !value),
  (* This could be optimized, as ~scalar_type never changes *)
  fun field ->
    let%bind fields_rev =
      match field, scalar_type with
      | Spec.Length_delimited data, `Fixed_64_bit ->
        read_fields ~f:(fun b -> Protobuffer.read_int64 b >>| Spec.fixed_64_bit) (Protobuffer.create data) []
      | Spec.Length_delimited data, `Fixed_32_bit ->
        read_fields ~f:(fun b -> Protobuffer.read_int32 b >>| Spec.fixed_32_bit) (Protobuffer.create data) []
      | Spec.Length_delimited data, `Varint  ->
        read_fields ~f:(fun b -> Protobuffer.read_varint b >>| Spec.varint) (Protobuffer.create data) []
      | Spec.Length_delimited _ as v, `Not_scalar -> return [v]
      | Spec.Fixed_32_bit _ as v, _  -> return [v]
      | Spec.Fixed_64_bit _ as v, _ -> return [v]
      | Spec.Varint _ as v, _ -> return [v]
    in
    List.fold_right ~init:Result.ok_unit ~f:(fun field acc->
        let%bind () = acc in
        read_field field
      ) fields_rev



let scalar_type: type a. a spec -> 'b = function
    | Double -> `Fixed_64_bit
    | Float -> `Fixed_32_bit
    | Int32 -> `Varint
    | Int64 -> `Varint
    | UInt32 -> `Varint
    | UInt64 -> `Varint
    | SInt32 -> `Varint
    | SInt64 -> `Varint
    | Fixed32 -> `Fixed_32_bit
    | Fixed64 -> `Fixed_64_bit
    | SFixed32 -> `Fixed_32_bit
    | SFixed64 -> `Fixed_64_bit
    | Bool -> `Varint
    | Enum _ -> `Varint
    | _ -> `Not_scalar

let rec sentinal : type a. a spec -> a sentinal * decoder = function
  | Double -> double_sentinal ()
  | Float -> float_sentinal ()
  | Int32 -> int32_sentinal ()
  | Int64 -> int64_sentinal ()
  | UInt32 -> uint32_sentinal ()
  | UInt64 -> uint64_sentinal ()
  | SInt32 -> sint32_sentinal ()
  | SInt64 -> sint64_sentinal ()
  | Fixed32 -> fixed32_sentinal ()
  | Fixed64 -> fixed64_sentinal ()
  | SFixed32 -> sfixed32_sentinal ()
  | SFixed64 -> sfixed64_sentinal ()
  | Bool -> bool_sentinal ()
  | String -> string_sentinal ()
  | Bytes -> bytes_sentinal ()
  | Message deser -> message_sentinal deser
  | Enum deser -> enum_sentinal deser
  | Repeated tpe ->
    let sentinal = sentinal tpe in
    repeated_sentinal ~scalar_type:(scalar_type tpe) sentinal
