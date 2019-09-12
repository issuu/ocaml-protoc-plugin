(** Some buffer to hold data, and to read and write data *)

open Core
open Spec
open Result.Let_syntax
let incr = 128

type t = {
  mutable offset : int;
  mutable data : Bytes.t;
}

type error = [ `Premature_end_of_input
             | `Unknown_field_type of int
             ] [@@deriving show]

let init ?(length = incr) () = {data = Bytes.create length; offset = 0}

(** Seems to be two different types here *)
let reset t =
  let data = Bytes.sub ~pos:0 ~len:t.offset t.data in
  { data; offset = 0 }

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

(** Return an error if there is not enough data in input *)
let validate_capacity t count =
  match Bytes.length t.data >= t.offset + count with
  | true -> Result.ok_unit
  | false -> Result.fail `Premature_end_of_input

(** Test if there is more data in the buffer to be read *)
let has_more t =
  t.offset < Bytes.length t.data

let write_byte t v =
  ensure_capacity t;
  Bytes.set t.data t.offset @@ Char.of_int_exn v;
  t.offset <- t.offset + 1

let read_byte t =
  let%bind () = validate_capacity t 1 in
  let v = Bytes.get t.data t.offset in
  t.offset <- t.offset + 1;
  return (Char.to_int v)

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

let read_varint t =
  let open Int64 in
  let rec inner acc =
    let%bind v = read_byte t in
    let v = of_int v in
    let acc = (v land 0x7FL) :: acc in
    match v > 127L with
    | true -> (* Still More data *)
      inner acc
    | false ->
      return acc
  in
  let%bind v = inner [] in
  let v = List.fold_left ~init:0L ~f:(fun acc c -> acc lsl 7 + c) v in
  return (to_int_exn v)

let write_field_header : t -> int -> int -> unit =
  fun t index field_type ->
  let header = (index lsl 3) + field_type in
  write_varint t header; ()

let read_field_header : t -> (int * int, error) result = fun t ->
  let%bind v = read_varint t in
  let tpe = v land 0x7 in
  let field_number = v / 8 in
  return (tpe, field_number)

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

let read_data t =
  let%bind size = read_varint t in
  let%bind () = validate_capacity t size in
  (* Create a substring *)
  let v = Bytes.sub t.data ~pos:t.offset ~len:size in
  t.offset <- t.offset + size;
  return (v |> Bytes.to_string)

(** Dont really know how to write an unsiged 32 bit *)
let write_int32 t v =
  let size = 4 in
  ensure_capacity ~cap:size t;
  EndianBytes.LittleEndian.set_int32 t.data t.offset v;
  t.offset <- t.offset + size

let read_int32 t =
  let size = 4 in
  let%bind () = validate_capacity t size in
  let v = EndianBytes.LittleEndian.get_int32 t.data t.offset in
  t.offset <- t.offset + size;
  return v

let write_int64 t v =
  let size = 8 in
  ensure_capacity ~cap:size t;
  EndianBytes.LittleEndian.set_int64 t.data t.offset v;
  t.offset <- t.offset + size

let read_int64 t =
  let size = 8 in
  let%bind () = validate_capacity t size in
  let v = EndianBytes.LittleEndian.get_int64 t.data t.offset in
  t.offset <- t.offset + size;
  return v

let write_field : t -> int -> field -> unit =
  fun t index field ->
  let field_type = match field with
    | Varint _ -> 0
    | Fixed_64_bit _ -> 1
    | Length_delimited _ -> 2
    | Fixed_32_bit _ -> 5
  in
  write_field_header t index field_type;
  match field with
  | Varint v -> write_varint t v
  | Fixed_64_bit v -> write_int64 t v
  | Length_delimited v -> write_data t v
  | Fixed_32_bit v -> write_int32 t v

let read_field : t -> (int * field, error) result = fun t ->
  let%bind (field_type, field_number) = read_field_header t in
  let%bind field =
    match field_type with
    | 0 ->
      let%bind v = read_varint t in
      return (Varint v)
    | 1 ->
      let%bind v = read_int64 t in
      return (Fixed_64_bit v)
    | 2 ->
      let%bind v = read_data t in
      return (Length_delimited v)
    | 5 ->
      let%bind v = read_int32 t in
      return (Fixed_32_bit v)
    | n ->
      Result.fail (`Unknown_field_type n)
  in
  return (field_number, field)

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
