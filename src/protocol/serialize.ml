open Core_kernel

(* Everything is encapsulated in messages,
   so everything will have a field id and a type assigned to it.
*)
type field =
  | Varint of int (* int32, int64, uint32, uint64, sint32, sint64, bool, enum *)
  | Fixed_64_bit of Int64.t (* fixed64, sfixed64, double *)
  | Length_delimited of string (* string, bytes, embedded messages, packed repeated fields *)
  | Fixed_32_bit of Int32.t (* fixed32, sfixed32, float *)

(** Some buffer to hold data, and to read and write data *)
module Buffer = struct
  let incr = 128
  type t = { mutable offset: int;
             mutable data: Bytes.t;
           }

  let init ?(length=incr) () =
    { data = Bytes.create length;
      offset = 0;
    }

  let contents t = Bytes.sub ~pos:0 ~len:t.offset t.data |> Bytes.to_string

  let ensure_capacity ?(cap=1) t =
    let length = Bytes.length t.data in
    let remain = length - t.offset in
    match cap - remain with
    | n when n <= 0 -> ()
    | n ->
        let data' = Bytes.create (length + max n incr) in
        Bytes.blit ~src:t.data ~src_pos:0 ~dst:data' ~dst_pos:0 ~len:length;
        t.data <- data'

  let add_byte t v =
    ensure_capacity t;
    Bytes.set t.data t.offset @@ Char.of_int_exn v;
    t.offset <- t.offset + 1

  let write_field_header: t -> int -> field -> unit = fun t index field ->
    let field_type_id = match field with
      | Varint _ -> 1
      | Fixed_64_bit _ -> 2
      | Length_delimited _ -> 4
      | Fixed_32_bit _ -> 5
    in
    let header = index lsl 3 + field_type_id in
    (* msg, lsb *)
    add_byte t (header lsr 8 land 0xff);
    add_byte t (header land 0xff);
    ()

  let rec write_varint t v =
    match v land 0x7F, v lsr 7 with
    | v, 0 -> add_byte t v
    | v, rem ->
        add_byte t (v lor 0x80);
        write_varint t rem

  let write_data t v =
    write_varint t (String.length v);
    ensure_capacity ~cap:(String.length v) t;
    Bytes.blit ~src:(Bytes.of_string v) ~src_pos:0 ~dst:t.data ~dst_pos:t.offset ~len:(String.length v);
    t.offset <- t.offset + (String.length v)

  (* This should not be here.
  let write_varint_signed t v =
    let v =
      match v with
      | v when v < 0 -> (((v * -1) - 1) * 2) + 1
      | v -> v * 2
    in
    write_varint t v
  *)

  (** Dont really know how to write an unsiged 32 bit *)
  let write_int32 t v =
    ensure_capacity ~cap:4 t;
    EndianBytes.LittleEndian.set_int32 t.data t.offset v;
    t.offset <- t.offset + 4

  let write_int64 t v =
    ensure_capacity ~cap:8 t;
    EndianBytes.LittleEndian.set_int64 t.data t.offset v;
    t.offset <- t.offset + 8

  let write_field : t -> int -> field -> unit = fun t index field ->
    write_field_header t index field;
    match field with
    | Varint v ->
      write_varint t v
    | Fixed_64_bit v ->
      write_int64 t v
    | Length_delimited v ->
      write_data t v
    | Fixed_32_bit v ->
      write_int32 t v
end

let serialize_field : int -> field -> unit =
 fun _ -> function
  | Varint v -> ignore v; failwith "Not implemented"
  | Length_delimited s -> ignore s; failwith "Not implemented"
  | Fixed_32_bit f -> ignore f; failwith "Not implemented"
  | Fixed_64_bit f -> ignore f; failwith "Not implemented"

(* Take a list of fields and return a field *)
let serialize_message: (int * field) list -> string = fun fields ->
  let buffer = Buffer.init () in
  List.iter ~f:(fun (index, field) -> Buffer.write_field buffer index field) fields;
  Buffer.contents buffer

type (_) protobuf_type =
  | Double: float protobuf_type
  | Float: float protobuf_type
  | Int64: int protobuf_type
  | Uint64: int protobuf_type
  | Int32: int protobuf_type
  | Fixed64: int protobuf_type
  | Fixed32: int protobuf_type
  | Sfixed32: int protobuf_type
  | Sfixed64: int protobuf_type
  | Sint32: int protobuf_type
  | Sint64: int protobuf_type
  | Uint32: int protobuf_type
  | Bool: bool protobuf_type
  | String: string protobuf_type
  | Bytes: bytes protobuf_type
  | Message: ('a -> string) -> 'a option protobuf_type
  | Enum: ('a -> int) -> 'a protobuf_type
  | Repeated: 'a protobuf_type -> 'a list protobuf_type

(* Lets create a list of these fields *)
type (_,_) protobuf_type_list =
  | Nil : ('a,'a) protobuf_type_list
  | Cons : (int * 'a protobuf_type) * ('b, 'c) protobuf_type_list -> (('a -> 'b), 'c) protobuf_type_list

let (^::) a b = Cons (a, b)

let signed_varint v =
  let v = match v with
    | v when v < 0 -> (((v * -1) - 1) * 2) + 1
    | v -> v * 2
  in
  Varint v

let unsigned_varint v =
  Varint v

(** Allow emitted code to present a protobuf specification. *)
let rec protocol_f: type a. Buffer.t -> (a, string) protobuf_type_list -> a = fun buffer -> function
  | Nil -> Buffer.contents buffer
  | Cons ((index, Double), rest) ->
    fun v ->
      (match v with 0.0 -> () | v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.bits_of_float v)));
      protocol_f buffer rest
  | Cons ((index, Float), rest) ->
    fun v ->
      (match v with 0.0 -> () | v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.bits_of_float v)));
      protocol_f buffer rest
  | Cons ((index, Int64), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (signed_varint v));
      protocol_f buffer rest
  | Cons ((index, Uint64), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (unsigned_varint v));
      protocol_f buffer rest
  | Cons ((index, Int32), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (signed_varint v));
      protocol_f buffer rest
  | Cons ((index, Fixed32), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)));
      protocol_f buffer rest
  | Cons ((index, Fixed64), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)));
      protocol_f buffer rest
  | Cons ((index, Sint32), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (signed_varint v));
      protocol_f buffer rest
  | Cons ((index, Sint64), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (signed_varint v));
      protocol_f buffer rest
  | Cons ((index, Uint32), rest) ->
    fun v ->
      (match v with 0 -> () | v -> Buffer.write_field buffer index (unsigned_varint v));
      protocol_f buffer rest
  | Cons ((index, Bool), rest) ->
    fun v ->
      (match v with false -> () | true -> Buffer.write_field buffer index (unsigned_varint 1));
      protocol_f buffer rest
  | Cons ((index, String), rest) ->
    fun v ->
      (match v with "" -> () | s -> Buffer.write_field buffer index (Length_delimited s));
      protocol_f buffer rest
  | Cons ((index, Bytes), rest) ->
    fun v ->
      Buffer.write_field buffer index (Length_delimited (Bytes.to_string v));
      protocol_f buffer rest
  | _ -> failwith "Not implemented"

(** Allright. now. Ready to emit specs *)
