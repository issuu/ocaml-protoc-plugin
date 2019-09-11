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


(** Some buffer to hold data, and to read and write data *)
module Buffer = struct
  let incr = 128

  type t = {
    mutable offset : int;
    mutable data : Bytes.t;
  }

  let init ?(length = incr) () = {data = Bytes.create length; offset = 0}

  let contents t = Bytes.sub ~pos:0 ~len:t.offset t.data |> Bytes.to_string

  let ensure_capacity ?(cap = 1) t =
    let length = Bytes.length t.data in
    let remain = length - t.offset in
    match cap - remain with
    | n when n <= 0 -> ()
    | n ->
      let data' = Bytes.create (length + max n incr) in
      Bytes.blit ~src:t.data ~src_pos:0 ~dst:data' ~dst_pos:0 ~len:t.offset;
      t.data <- data'

  let add_byte t v =
    ensure_capacity t;
    Bytes.set t.data t.offset @@ Char.of_int_exn v;
    t.offset <- t.offset + 1

  let write_varint t v =
    let rec inner t v =
      let open Int64 in
      match v land 0x7FL, v lsr 7 with
      | v, 0L -> add_byte t (v |> to_int_exn)
      | v, rem ->
        add_byte t (v lor 0x80L |> to_int_exn);
        inner t rem
    in
    inner t (Int64.of_int v)

  let write_field_header : t -> int -> field -> unit =
   fun t index field ->
    let field_type_id =
      match field with
      | Varint _ -> 0
      | Fixed_64_bit _ -> 1
      | Length_delimited _ -> 2
      | Fixed_32_bit _ -> 5
    in
    let header = (index lsl 3) + field_type_id in
    write_varint t header; ()

  let write_data t v =
    write_varint t (String.length v);
    ensure_capacity ~cap:(String.length v) t;
    Bytes.blit
      ~src:(Bytes.of_string v)
      ~src_pos:0
      ~dst:t.data
      ~dst_pos:t.offset
      ~len:(String.length v);
    t.offset <- t.offset + String.length v

  (** Dont really know how to write an unsiged 32 bit *)
  let write_int32 t v =
    ensure_capacity ~cap:4 t;
    EndianBytes.LittleEndian.set_int32 t.data t.offset v;
    t.offset <- t.offset + 4

  let write_int64 t v =
    ensure_capacity ~cap:8 t;
    EndianBytes.LittleEndian.set_int64 t.data t.offset v;
    t.offset <- t.offset + 8

  let write_field : t -> int -> field -> unit =
   fun t index field ->
    write_field_header t index field;
    match field with
    | Varint v -> write_varint t v
    | Fixed_64_bit v -> write_int64 t v
    | Length_delimited v -> write_data t v
    | Fixed_32_bit v -> write_int32 t v
end

(* This make a good inline test *)
let%test _ =
  let buffer = Buffer.init () in
  Buffer.write_field buffer 1 (Varint 1);
  let c = Buffer.contents buffer in
  String.length c = 2 && c = "\x08\x01"

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
 fun fields ->
  let buffer = Buffer.init () in
  List.iter ~f:(fun (index, field) -> Buffer.write_field buffer index field) fields;
  Buffer.contents buffer

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

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Buffer.t -> (a, string) protobuf_type_list -> a =
 fun buffer -> function
  | Nil -> Buffer.contents buffer
  | Cons ((index, Double), rest) ->
    fun v ->
      (match v with
      | 0.0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.bits_of_float v)));
      serialize buffer rest
  | Cons ((index, Float), rest) ->
    fun v ->
      (match v with
      | 0.0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.bits_of_float v)));
      serialize buffer rest
  | Cons ((index, Int64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, UInt64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Int32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v ->
        let v = v land 0xffffffff in
        Buffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Fixed32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, Fixed64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SFixed32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SFixed64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SInt32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (signed_varint v));
      serialize buffer rest
  | Cons ((index, SInt64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (signed_varint v));
      serialize buffer rest
  | Cons ((index, UInt32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Buffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Bool), rest) ->
    fun v ->
      (match v with
      | false -> ()
      | true -> Buffer.write_field buffer index (unsigned_varint 1));
      serialize buffer rest
  | Cons ((index, String), rest) ->
    fun v ->
      (match v with
      | "" -> ()
      | s -> Buffer.write_field buffer index (Length_delimited s));
      serialize buffer rest
  | Cons ((index, Bytes), rest) ->
    fun v ->
      Buffer.write_field buffer index (Length_delimited (Bytes.to_string v));
      serialize buffer rest
  | Cons ((index, Message to_string), rest) ->
    fun v ->
      (match v with
      | None -> ()
      | Some msg -> Buffer.write_field buffer index (Length_delimited (to_string msg)));
      serialize buffer rest
  | Cons ((index, Enum to_int), rest) ->
    fun v ->
      (match to_int v with
      | 0 -> ()
      | n -> Buffer.write_field buffer index (unsigned_varint n));
      serialize buffer rest
  | Cons ((_index, Oneof f), rest) ->
    (* Oneof fields ignores the initial index *)
    fun v ->
     let index, v = f v in
     Buffer.write_field buffer index v;
     serialize buffer rest
  (* Repeated fields - Not packed *)
  | Cons ((index, Repeated (Message to_string)), rest) ->
    fun v ->
      List.iter v ~f:(function
          | None -> failwith "Repeated message cannot be null"
          | Some msg ->
            Buffer.write_field buffer index (Length_delimited (to_string msg)));
      serialize buffer rest
  | Cons ((index, Repeated String), rest) ->
    fun v ->
      List.iter v ~f:(fun msg -> Buffer.write_field buffer index (Length_delimited msg));
      serialize buffer rest
  | Cons ((index, Repeated Bytes), rest) ->
    fun v ->
      List.iter v ~f:(fun msg ->
          Buffer.write_field buffer index (Length_delimited (Bytes.to_string msg)));
      serialize buffer rest
  (* Repeated packed fields *)
  | Cons ((index, Repeated Double), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v ->
          Buffer.write_field value_buffer index (Fixed_64_bit (Int64.bits_of_float v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Float), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v ->
          Buffer.write_field value_buffer index (Fixed_32_bit (Int32.bits_of_float v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Int64), rest) ->
    (* TODO: This should not be encoded as a signed varint, but we should handle negative numbers here *)
    fun vs ->
     let value_buffer = Buffer.init () in
     List.iter ~f:(fun v -> Buffer.write_field buffer index (signed_varint v)) vs;
     Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
     serialize buffer rest
  | Cons ((index, Repeated UInt64), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter ~f:(fun v -> Buffer.write_field buffer index (unsigned_varint v)) vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Int32), rest) ->
    (* TODO: This should not be encoded as a signed varint, but we should handle negative numbers here *)
    fun vs ->
     let value_buffer = Buffer.init () in
     List.iter ~f:(fun v -> Buffer.write_field buffer index (unsigned_varint v)) vs;
     Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
     serialize buffer rest
  | Cons ((index, Repeated Fixed32), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Fixed64), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SFixed32), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SFixed64), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SInt32), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter ~f:(fun v -> Buffer.write_field buffer index (signed_varint v)) vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SInt64), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter ~f:(fun v -> Buffer.write_field buffer index (signed_varint v)) vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated UInt32), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter ~f:(fun v -> Buffer.write_field buffer index (unsigned_varint v)) vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Bool), rest) ->
    let int_of_bool = function
      | false -> 0
      | true -> 1
    in
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (unsigned_varint (int_of_bool v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated (Enum to_int)), rest) ->
    fun vs ->
      let value_buffer = Buffer.init () in
      List.iter
        ~f:(fun v -> Buffer.write_field buffer index (unsigned_varint (to_int v)))
        vs;
      Buffer.write_field buffer index (Length_delimited (Buffer.contents value_buffer));
      serialize buffer rest
  | Cons ((_, Repeated (Repeated _)), _) ->
    failwith "Chained repeated fields not supported"
  | Cons ((_, Repeated (Oneof _)), _) -> failwith "Oneof fields cannot be repeated"

let serialize spec = serialize (Buffer.init ()) spec
