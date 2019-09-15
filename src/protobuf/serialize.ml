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
  | Message : ('a -> Writer.t) -> 'a option spec
  | Enum : ('a -> int) -> 'a spec
  | Repeated : 'a spec -> 'a list spec
  | RepeatedMessage: ('a -> Writer.t) -> 'a list spec
  | Oneof : ('a -> int * field) -> 'a spec

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
  fun fields ->
  let writer = Writer.init () in
  List.iter ~f:(fun (index, field) -> Writer.write_field writer index field) fields;
  Writer.contents writer

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
let field_of_string v = Length_delimited { offset = 0; length = String.length v; data = v }
let field_of_bytes v = Length_delimited { offset = 0; length = Bytes.length v; data = Bytes.to_string v }
let field_of_message ~f v =
  let data = match v with
    | None -> ""
    | Some v -> (f v)
  in
  field_of_string data
let field_of_enum ~f v =
  field_of_uint64 (f v)

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Writer.t -> (a, Writer.t) protobuf_type_list -> a =
  (* This function could just test for default values, and choose not to write them *)
  let write_field writer ~index ~f rest v =
    (* Only actually write if the value is different from the default value *)
    let () = match f v with
      | Varint 0 -> ()
      | Fixed_64_bit v when v = Int64.zero -> ()
      | Fixed_32_bit v when v = Int32.zero -> ()
      | Length_delimited { length = 0; _ } -> ()
      | field ->
        Writer.write_field writer index field
    in
    serialize writer rest
  in
  let write_packed_field writer ~index ~f rest vs =
    let value_writer = Writer.init () in
    List.iter
      ~f:(fun v -> Writer.add_field value_writer (f v))
      vs;
    Writer.concat_as_length_delimited writer ~src:value_writer index;
    serialize writer rest
  in
  let write_message writer ~index ~f rest msg =
    let () = match msg with
      | None -> ()
      | Some msg ->
        let message_writer = f msg in
        Writer.concat_as_length_delimited writer ~src:message_writer index;
    in
    serialize writer rest
  in
  fun writer -> function
    | Nil -> writer
    | Cons ((index, Double), rest) ->
      write_field writer ~index ~f:field_of_double rest
    | Cons ((index, Float), rest) ->
      write_field writer ~index ~f:field_of_float rest
    | Cons ((index, Int64), rest) ->
      write_field writer ~index ~f:field_of_int64 rest
    | Cons ((index, UInt64), rest) ->
      write_field writer ~index ~f:field_of_uint64 rest
    | Cons ((index, SInt64), rest) ->
      write_field writer ~index ~f:field_of_sint64 rest
    | Cons ((index, Int32), rest) ->
      write_field writer ~index ~f:field_of_int32 rest
    | Cons ((index, UInt32), rest) ->
      write_field writer ~index ~f:field_of_uint32 rest
    | Cons ((index, SInt32), rest) ->
      write_field writer ~index ~f:field_of_sint32 rest
    | Cons ((index, Fixed32), rest) ->
      write_field writer ~index ~f:field_of_fixed32 rest
    | Cons ((index, Fixed64), rest) ->
      write_field writer ~index ~f:field_of_fixed64 rest
    | Cons ((index, SFixed32), rest) ->
      write_field writer ~index ~f:field_of_sfixed32 rest
    | Cons ((index, SFixed64), rest) ->
      write_field writer ~index ~f:field_of_sfixed64 rest
    | Cons ((index, Bool), rest) ->
      write_field writer ~index ~f:field_of_bool rest
    | Cons ((index, String), rest) ->
      write_field writer ~index ~f:field_of_string rest
    | Cons ((index, Bytes), rest) ->
      write_field writer ~index ~f:field_of_bytes rest
    | Cons ((index, Message to_writer), rest) ->
      write_message writer ~index ~f:to_writer rest
    | Cons ((index, Enum to_int), rest) ->
      write_field writer ~index ~f:(field_of_enum ~f:to_int) rest
    | Cons ((_index, Oneof f), rest) ->
      (* Oneof fields ignores the initial index *)
      fun v ->
        let index, v = f v in
        Writer.write_field writer index v;
        serialize writer rest
    (* Repeated fields - Not packed *)
    | Cons ((index, RepeatedMessage to_writer), rest) ->
      fun vs ->
        let writer = List.fold_left ~init:writer ~f:(fun writer v ->
            write_message writer ~index ~f:to_writer Nil (Some v)) vs
        in
        serialize writer rest
    | Cons ((_, Repeated (Message _)), _) ->
      failwith "Illegal construct";
    | Cons ((index, Repeated String), rest) ->
      fun v ->
        List.iter v ~f:(fun msg -> Writer.write_field writer index (field_of_string msg));
        serialize writer rest
    | Cons ((index, Repeated Bytes), rest) ->
      fun v ->
        List.iter v ~f:(fun msg -> Writer.write_field writer index (field_of_bytes msg));
        serialize writer rest
    (* Repeated fields - Packed *)
    | Cons ((index, Repeated Double), rest) ->
      write_packed_field writer ~index ~f:field_of_double rest
    | Cons ((index, Repeated Float), rest) ->
      write_packed_field writer ~index ~f:field_of_float rest
    | Cons ((index, Repeated Int64), rest) ->
      write_packed_field writer ~index ~f:field_of_int64 rest
    | Cons ((index, Repeated Int32), rest) ->
      write_packed_field writer ~index ~f:field_of_int32 rest
    | Cons ((index, Repeated UInt64), rest) ->
      write_packed_field writer ~index ~f:field_of_uint64 rest
    | Cons ((index, Repeated UInt32), rest) ->
      write_packed_field writer ~index ~f:field_of_uint32 rest
    | Cons ((index, Repeated SInt64), rest) ->
      write_packed_field writer ~index ~f:field_of_sint64 rest
    | Cons ((index, Repeated SInt32), rest) ->
      write_packed_field writer ~index ~f:field_of_sint32 rest
    | Cons ((index, Repeated Fixed64), rest) ->
      write_packed_field writer ~index ~f:field_of_fixed64 rest
    | Cons ((index, Repeated Fixed32), rest) ->
      write_packed_field writer ~index ~f:field_of_fixed32 rest
    | Cons ((index, Repeated SFixed64), rest) ->
      write_packed_field writer ~index ~f:field_of_sfixed64 rest
    | Cons ((index, Repeated SFixed32), rest) ->
      write_packed_field writer ~index ~f:field_of_sfixed32 rest
    | Cons ((index, Repeated Bool), rest) ->
      write_packed_field writer ~index ~f:field_of_bool rest
    | Cons ((index, Repeated (Enum to_int)), rest) ->
      write_packed_field writer ~index ~f:(fun v -> to_int v |> field_of_uint64) rest
    | Cons ((_, Repeated (Repeated _)), _) ->
      failwith "Chained repeated fields are not allowed"
    | Cons ((_, Repeated (RepeatedMessage _)), _) ->
      failwith "Chained repeated fields are not allowed"
    | Cons ((_, Repeated (Oneof _)), _) ->
      failwith "Oneof fields cannot be repeated"

let serialize spec = serialize (Writer.init ()) spec
