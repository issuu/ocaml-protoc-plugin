open Base
open Result.Let_syntax

type error =
  [ Reader.error
  | `Wrong_field_type of string * Spec.field
  | `Illegal_value of string * Spec.field
  | `Not_implemented
  | `Unknown_enum_value of int
  | `Oneof_missing ]
[@@deriving show]

(** Module for deserializing values *)
type nonrec 'a result = ('a, error) Result.t

type _ spec =
  | Double : float spec
  | Float : float spec
  | Int32 : int spec
  | Int64 : int spec
  | UInt32 : int spec
  | UInt64 : int spec
  | SInt32 : int spec
  | SInt64 : int spec
  | Fixed32 : int spec
  | Fixed64 : int spec
  | SFixed32 : int spec
  | SFixed64 : int spec
  | Bool : bool spec
  | String : string spec
  | Bytes : bytes spec
  | Enum : (int -> 'a result) -> 'a spec

type _ compound =
  | Message: (Reader.t -> 'a result) -> 'a option compound
  | RepeatedMessage : (Reader.t -> 'a result) -> 'a list compound
  | Repeated : 'a spec -> 'a list compound
  | Basic : 'a spec -> 'a compound

type 'a sentinal = unit -> 'a

type decoder = Spec.field -> unit result

let error_wrong_field str field : _ result =
  `Wrong_field_type (str, field) |> Result.fail

let error_illegal_value str field : _ result = `Illegal_value (str, field) |> Result.fail

(** Deserialize a reader. *)
let read_fields : Reader.t -> (int * Spec.field) list result =
 fun t ->
  let rec inner acc =
    match Reader.has_more t with
    | false -> return (List.rev acc) (* Preserve order *)
    | true ->
      let%bind v = Reader.read_field t in
      inner (v :: acc)
  in
  match inner [] with
  | Ok v -> return v
  | Error `Premature_end_of_input -> Result.fail `Premature_end_of_input
  | Error (`Unknown_field_type n) -> Result.fail (`Unknown_field_type n)

(** Deserialize takes a list of field sentinals and a reader for the
    data. Data is decoded into a list of tags, fields
    and the appropiate function for the tag is called.
    After all fields have been processed,
    data can be read from the sentinals, created by the caller

    A helper function exists to create sentinals
*)
let deserialize : (int * decoder) list -> Reader.t -> unit result =
 fun spec reader ->
  (* Exceptions here is an error in code-generation. Crash hard on that! *)
 let decoder_map = Map.of_alist_exn (module Int) spec in
 let%bind fields = read_fields reader in
 List.fold_left
   ~init:(Result.Ok ())
   ~f:(fun acc (field_idx, field_val) ->
       let%bind () = acc in
       match Map.find decoder_map field_idx with
       | None -> return ()
       | Some f -> f field_val)
   fields

let read_varint ~signed ~type_name = function
  | Spec.Varint v -> begin
      let v = match signed with
        | true when v % 2 = 0 -> v / 2
        | true -> (v / 2 * -1) - 1
        | false -> v
      in
      return v
    end
  | field -> error_wrong_field type_name field

let ok_exn = function
  | Ok v -> v
  | Error _ -> failwith "Must contain a usable value"

let type_of_spec: type a. a spec -> a * 'b * (Spec.field -> a result) = function
  | Double -> (0.0, `Fixed_64_bit, function Spec.Fixed_64_bit v -> return (Int64.float_of_bits v) | field -> error_wrong_field "double" field)
  | Float -> (0.0, `Fixed_32_bit, function Spec.Fixed_32_bit v -> return (Int32.float_of_bits v) | field -> error_wrong_field "float" field)
  | Int32 -> (0, `Varint, fun field ->
      let%bind v = read_varint ~signed:false ~type_name:"int32" field in
      return (match v lsr 31 with 1 -> v lor 0x7fffffff00000000 | _ -> v)
    )
  | Int64 -> (0, `Varint, read_varint ~signed:false ~type_name:"int64")
  | UInt32 -> (0, `Varint, read_varint ~signed:false ~type_name:"uint32")
  | UInt64 -> (0, `Varint, read_varint ~signed:false ~type_name:"uint64")
  | SInt32 -> (0, `Varint, read_varint ~signed:true ~type_name:"sint32")
  | SInt64 -> (0, `Varint, read_varint ~signed:true ~type_name:"sint64")
  | Fixed32 -> (0, `Fixed_32_bit, function Spec.Fixed_32_bit v -> return (Int32.to_int_exn v) | field -> error_wrong_field "fixed32" field)
  | Fixed64 -> (0, `Fixed_64_bit, function Spec.Fixed_64_bit v -> return (Int64.to_int_exn v) | field -> error_wrong_field "fixed64" field)
  | SFixed32 -> (0, `Fixed_32_bit, function Spec.Fixed_32_bit v -> return (Int32.to_int_exn v) | field -> error_wrong_field "sfixed32" field)
  | SFixed64 -> (0, `Fixed_64_bit, function Spec.Fixed_64_bit v -> return (Int64.to_int_exn v) | field -> error_wrong_field "sfixed64" field)
  | Bool -> (false, `Varint, function Spec.Varint v -> return (v <> 0) | field -> error_wrong_field "bool" field)
  | Enum of_int -> (of_int 0 |> ok_exn, `Varint, function Spec.Varint v -> of_int v | field -> error_wrong_field "enum" field)
  | String -> ("", `Length_delimited, function Spec.Length_delimited {offset; length; data} -> return (String.sub ~pos:offset ~len:length data)
                                             | field -> error_wrong_field "string" field)
  | Bytes -> (Bytes.create 0, `Length_delimited, function Spec.Length_delimited {offset; length; data} ->
      return (String.sub ~pos:offset ~len:length data |> Bytes.of_string)
                                            | field -> error_wrong_field "string" field)

let sentinal: type a. a compound -> a sentinal * decoder = function
  | Basic spec ->
    let default, _, read = type_of_spec spec in
    let v = ref default in
    let get () = !v in
    let read field =
      let%bind value = read field in
      v := value;
      return ()
    in
    get, read

  | Message deser ->
    let v = ref None in
    let get () = !v in
    let read = function
      | Spec.Length_delimited {offset; length; data} ->
        let reader = Reader.create ~length ~offset data in
        let%bind message = deser reader in
        v := Some message;
        return ()
      | field -> error_wrong_field "message" field
    in
    (get, read)

  | RepeatedMessage deser ->
    let v = ref [] in
    let get () = List.rev !v in
    let read = function
      | Spec.Length_delimited {offset; length; data} ->
        let reader = Reader.create ~length ~offset data in
        let%bind message = deser reader in
        v := message :: !v;
        return ()
      | field -> error_wrong_field "message" field
    in
    (get, read)

  | Repeated spec ->
    let read_field = function
      | `Length_delimited -> None
      | `Varint -> Some Reader.read_varint
      | `Fixed_64_bit -> Some Reader.read_fixed64
      | `Fixed_32_bit -> Some Reader.read_fixed32
    in
    let rec read_repeated reader decode read_f = match Reader.has_more reader with
      | false -> return ()
      | true ->
        let%bind field = decode reader in
        let%bind () = read_f field in
        read_repeated reader decode read_f
    in
    let (_, field_type, read_type) = type_of_spec spec in
    let v = ref [] in
    let get () = List.rev !v in
    let rec read field = match field, read_field field_type with
      | (Spec.Length_delimited _ as field), None ->
        let%bind v' = read_type field in
        v := v' :: !v;
        return ()
      | Spec.Length_delimited { offset; length; data }, Some read_field ->
        read_repeated (Reader.create ~offset ~length data) read_field read
      | field, _ -> let%bind v' = read_type field in
        v := v' :: !v;
        return ()
    in
    get, read


(** Oneofs are handled a bit special. *)
type _ oneof =
  | Oneof : (int * 'b compound * ('b -> 'a)) -> 'a oneof

let oneof_sentinal: 'a oneof list -> (unit -> 'a result) * (int * decoder) list = fun oneofs ->
  let value = ref None in
  let create_sentinal: 'a oneof -> (int * decoder) = function
    | Oneof (index, compound, constr) ->
      let (get, read) = sentinal compound in
      let read field =
        let%bind () = read field in
        let v = get () in
        value := Some (constr v);
        return ()
      in
      index, read
  in
  let read () =
    match !value with
    | None -> Result.fail `Oneof_missing
    | Some v -> return v
  in
    let decoders = List.map ~f:create_sentinal oneofs in
    (read, decoders)
