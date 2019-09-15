(** Some buffer to hold data, and to read and write data *)

open Core
open Spec
let incr = 128

type t = {
  mutable offset : int;
  mutable data : Bytes.t;
}

type error = [ `Premature_end_of_input
             | `Unknown_field_type of int
             ] [@@deriving show]

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

let write_byte t v =
  ensure_capacity t;
  Bytes.set t.data t.offset @@ Char.of_int_exn v;
  t.offset <- t.offset + 1

let write_varint t v =
  let rec inner t v =
    let open Int64 in
    match v land 0x7FL, v lsr 7 with
    | v, 0L -> write_byte t (v |> to_int_exn)
    | v, rem ->
      write_byte t (v lor 0x80L |> to_int_exn);
      inner t rem
  in
  inner t (Int64.of_int v)

let write_field_header : t -> int -> int -> unit =
  fun t index field_type ->
  let header = (index lsl 3) + field_type in
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
  let size = 4 in
  ensure_capacity ~cap:size t;
  EndianBytes.LittleEndian.set_int32 t.data t.offset v;
  t.offset <- t.offset + size

let write_int64 t v =
  let size = 8 in
  ensure_capacity ~cap:size t;
  EndianBytes.LittleEndian.set_int64 t.data t.offset v;
  t.offset <- t.offset + size

let write_raw_field: t -> field -> unit = fun t -> function
  | Varint v -> write_varint t v
  | Fixed_64_bit v -> write_int64 t v
  | Length_delimited v -> write_data t v
  | Fixed_32_bit v -> write_int32 t v

let write_field : t -> int -> field -> unit =
  fun t index field ->
  let field_type = match field with
    | Varint _ -> 0
    | Fixed_64_bit _ -> 1
    | Length_delimited _ -> 2
    | Fixed_32_bit _ -> 5
  in
  write_field_header t index field_type;
  write_raw_field t field

let dump t =
  Bytes.to_list t.data
  |> List.map ~f:Char.to_int
  |> List.map ~f:(sprintf "%02x")
  |> String.concat ~sep:"-"
  |> printf "Buffer: %s\n"

let%test _ =
  let buffer = init () in
  write_field buffer 1 (Varint 1);
  let c = contents buffer in
  String.length c = 2 && c = "\x08\x01"
