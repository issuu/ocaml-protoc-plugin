open Core_kernel
open Spec

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
  | Message : ('a -> string) -> 'a option spec
  | Enum : ('a -> int) -> 'a spec
  | Repeated : 'a spec -> 'a list spec
  | Oneof : ('a -> int * field) -> 'a spec

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
  fun fields ->
  let buffer = Protobuffer.init () in
  List.iter ~f:(fun (index, field) -> Protobuffer.write_field buffer index field) fields;
  Protobuffer.contents buffer

type (_, _) protobuf_type_list =
  | Nil : ('a, 'a) protobuf_type_list
  | Cons :
      (int * 'a spec) * ('b, 'c) protobuf_type_list
      -> ('a -> 'b, 'c) protobuf_type_list

let ( ^:: ) a b = Cons (a, b)

let unsigned_varint v = Varint v

let signed_varint v =
  let v =
    match v with
    | v when v < 0 -> (((v * -1) - 1) * 2) + 1
    | v -> v * 2
  in
  Varint v

let%test _ = signed_varint 0 = Varint 0

let%test _ = signed_varint (-1) = Varint 1

let%test _ = signed_varint 1 = Varint 2

let%test _ = signed_varint (-2) = Varint 3

let%test _ = signed_varint 2147483647 = Varint 4294967294

let%test _ = signed_varint (-2147483648) = Varint 4294967295

let field_of_double v = Fixed_64_bit (Int64.bits_of_float v)
let field_of_float v = Fixed_32_bit (Int32.bits_of_float v)
let field_of_int64 v = unsigned_varint v
let field_of_int32 v = unsigned_varint (v land 0xffffffff)
let field_of_sint64 v = signed_varint v
let field_of_sint32 v = signed_varint v
let field_of_uint64 v = unsigned_varint v
let field_of_uint32 v = unsigned_varint v
let field_of_fixed64 v = Fixed_64_bit (Int64.of_int_exn v)
let field_of_fixed32 v = Fixed_32_bit (Int32.of_int_exn v)
let field_of_sfixed64 v = Fixed_64_bit (Int64.of_int_exn v)
let field_of_sfixed32 v = Fixed_32_bit (Int32.of_int_exn v)
let field_of_bool v = unsigned_varint (match v with true -> 1 | false -> 0)
let field_of_string v = Length_delimited v
let field_of_bytes v = Length_delimited (Bytes.to_string v)
let field_of_message ~f v =
  let data = match v with
    | None -> ""
    | Some v -> (f v)
  in
  Length_delimited data
let field_of_enum ~f v =
  field_of_uint64 (f v)

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Protobuffer.t -> (a, Protobuffer.t) protobuf_type_list -> a =
  (* This function could just test for default values, and choose not to write them *)
  let write_field buffer ~index ~f rest v =
    (* Only actually write if the value is different from the default value *)
    let () = match f v with
      | Varint 0 -> ()
      | Fixed_64_bit v when v = Int64.zero -> ()
      | Fixed_32_bit v when v = Int32.zero -> ()
      | Length_delimited "" -> ()
      | field ->
        Protobuffer.write_field buffer index field
    in
    serialize buffer rest
  in
  let write_packed_field buffer ~index ~f rest vs =
    let value_buffer = Protobuffer.init () in
    List.iter
      ~f:(fun v -> Protobuffer.write_raw_field value_buffer (f v))
      vs;
    Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
    serialize buffer rest
  in

  fun buffer -> function
    | Nil -> buffer
    | Cons ((index, Double), rest) ->
      write_field buffer ~index ~f:field_of_double rest
    | Cons ((index, Float), rest) ->
      write_field buffer ~index ~f:field_of_float rest
    | Cons ((index, Int64), rest) ->
      write_field buffer ~index ~f:field_of_int64 rest
    | Cons ((index, UInt64), rest) ->
      write_field buffer ~index ~f:field_of_uint64 rest
    | Cons ((index, SInt64), rest) ->
      write_field buffer ~index ~f:field_of_sint64 rest
    | Cons ((index, Int32), rest) ->
      write_field buffer ~index ~f:field_of_int32 rest
    | Cons ((index, UInt32), rest) ->
      write_field buffer ~index ~f:field_of_uint32 rest
    | Cons ((index, SInt32), rest) ->
      write_field buffer ~index ~f:field_of_sint32 rest
    | Cons ((index, Fixed32), rest) ->
      write_field buffer ~index ~f:field_of_fixed32 rest
    | Cons ((index, Fixed64), rest) ->
      write_field buffer ~index ~f:field_of_fixed64 rest
    | Cons ((index, SFixed32), rest) ->
      write_field buffer ~index ~f:field_of_sfixed32 rest
    | Cons ((index, SFixed64), rest) ->
      write_field buffer ~index ~f:field_of_sfixed64 rest
    | Cons ((index, Bool), rest) ->
      write_field buffer ~index ~f:field_of_bool rest
    | Cons ((index, String), rest) ->
      write_field buffer ~index ~f:field_of_string rest
    | Cons ((index, Bytes), rest) ->
      write_field buffer ~index ~f:field_of_bytes rest
    | Cons ((index, Message to_string), rest) ->
      write_field buffer ~index ~f:(field_of_message ~f:to_string) rest
    | Cons ((index, Enum to_int), rest) ->
      write_field buffer ~index ~f:(field_of_enum ~f:to_int) rest
    | Cons ((_index, Oneof f), rest) ->
      (* Oneof fields ignores the initial index *)
      fun v ->
        let index, v = f v in
        Protobuffer.write_field buffer index v;
        serialize buffer rest
    (* Repeated fields - Not packed *)
    | Cons ((index, Repeated (Message to_string)), rest) ->
      fun v ->
        List.iter v ~f:(function
            | None -> failwith "Repeated message cannot be null"
            | Some msg ->
              Protobuffer.write_field buffer index (Length_delimited (to_string msg)));
        serialize buffer rest
    | Cons ((index, Repeated String), rest) ->
      fun v ->
        List.iter v ~f:(fun msg -> Protobuffer.write_field buffer index (Length_delimited msg));
        serialize buffer rest
    | Cons ((index, Repeated Bytes), rest) ->
      fun v ->
        List.iter v ~f:(fun msg -> Protobuffer.write_field buffer index (Length_delimited (Bytes.to_string msg)));
        serialize buffer rest
    | Cons ((index, Repeated Double), rest) ->
      write_packed_field buffer ~index ~f:field_of_double rest
    | Cons ((index, Repeated Float), rest) ->
      write_packed_field buffer ~index ~f:field_of_float rest
    | Cons ((index, Repeated Int64), rest) ->
      write_packed_field buffer ~index ~f:field_of_int64 rest
    | Cons ((index, Repeated Int32), rest) ->
      write_packed_field buffer ~index ~f:field_of_int32 rest
    | Cons ((index, Repeated UInt64), rest) ->
      write_packed_field buffer ~index ~f:field_of_uint64 rest
    | Cons ((index, Repeated UInt32), rest) ->
      write_packed_field buffer ~index ~f:field_of_uint32 rest
    | Cons ((index, Repeated SInt64), rest) ->
      write_packed_field buffer ~index ~f:field_of_sint64 rest
    | Cons ((index, Repeated SInt32), rest) ->
      write_packed_field buffer ~index ~f:field_of_sint32 rest
    | Cons ((index, Repeated Fixed64), rest) ->
      write_packed_field buffer ~index ~f:field_of_fixed64 rest
    | Cons ((index, Repeated Fixed32), rest) ->
      write_packed_field buffer ~index ~f:field_of_fixed32 rest
    | Cons ((index, Repeated SFixed64), rest) ->
      write_packed_field buffer ~index ~f:field_of_sfixed64 rest
    | Cons ((index, Repeated SFixed32), rest) ->
      write_packed_field buffer ~index ~f:field_of_sfixed32 rest
    | Cons ((index, Repeated Bool), rest) ->
      write_packed_field buffer ~index ~f:field_of_bool rest
    | Cons ((index, Repeated (Enum to_int)), rest) ->
      write_packed_field buffer ~index ~f:(fun v -> to_int v |> field_of_uint64) rest
    | Cons ((_, Repeated (Repeated _)), _) ->
      failwith "Chained repeated fields not supported"
    | Cons ((_, Repeated (Oneof _)), _) ->
      failwith "Oneof fields cannot be repeated"

let serialize spec = serialize (Protobuffer.init ()) spec
