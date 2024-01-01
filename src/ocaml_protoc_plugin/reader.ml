(** Some buffer to hold data, and to read and write data *)

open StdLabels
open Field

type boxed = Boxed | Unboxed

type t = {
  mutable offset : int;
  end_offset : int;
  data : String.t;
}

let create ?(offset = 0) ?length data =
  let end_offset =
    match length with
    | None -> String.length data
    | Some l -> l + offset
  in
  assert (String.length data >= end_offset);
  {offset; end_offset; data}

[@@inline]
let validate_capacity t count =
  match t.offset + count <= t.end_offset with
  | true -> ()
  | false ->
    Result.raise `Premature_end_of_input

[@@inline]
let has_more t = t.offset < t.end_offset

[@@inline]
let read_byte t =
  validate_capacity t 1;
  let v = String.get_uint8 t.data t.offset in
  t.offset <- t.offset + 1;
  v

let read_raw_varint t =
  let open Infix.Int64 in
  let rec inner n acc =
    let v = Int64.of_int (read_byte t) in
    let acc = acc + (v land 0x7FL) lsl n in
    match v > 127L with
    | true ->
      (* Still More data *)
      inner (Int.add n 7) acc
    | false -> acc
  in
  inner 0 0L

let read_raw_varint_unboxed t =
  let rec inner n acc =
    let v = read_byte t in
    let acc = acc + (v land 0x7F) lsl n in
    match v > 127 with
    | true ->
      (* Still More data *)
      inner (n + 7) acc
    | false -> acc
  in
  inner 0 0

[@@inline]
let read_varint t = Varint (read_raw_varint t)
let read_varint_unboxed t = Varint_unboxed (read_raw_varint_unboxed t)

(* Implement little endian ourselves *)
let read_fixed32 t =
  let size = 4 in
  validate_capacity t size;
  let v = Bytes.get_int32_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  (Fixed_32_bit v)

let read_fixed64 t =
  let size = 8 in
  validate_capacity t size;
  let v = Bytes.get_int64_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  (Fixed_64_bit v)

let read_length_delimited t =
  let length = read_raw_varint_unboxed t in
  validate_capacity t length;
  let v = Length_delimited {offset = t.offset; length; data = t.data} in
  t.offset <- t.offset + length;
  v

let read_field_header : t -> int * int = fun t ->
  let v = read_raw_varint_unboxed t in
  let tpe = v land 0x7 in
  let field_number = v / 8 in
  (tpe, field_number)

let read_field_content = fun boxed ->
  let read_varint = match boxed with
    | Boxed -> read_varint
    | Unboxed -> read_varint_unboxed
  in
  function
  | 0 -> read_varint
  | 1 -> read_fixed64
  | 2 -> read_length_delimited
  | 5 -> read_fixed32
  | n -> fun _ -> Result.raise (`Unknown_field_type n)


let read_field : boxed -> t -> int * Field.t = fun boxed ->
  let read_field_content = read_field_content boxed in
  fun t ->
    let (field_type, field_number) = read_field_header t in
    field_number, read_field_content field_type t


let to_list: t -> (int * Field.t) list =
  let read_field = read_field Boxed in
  fun t ->
  (* Make this tailrec *)
  let[@tail_mod_cons] rec inner () = match has_more t with
    | true -> read_field t :: inner ()
    | false -> []
  in
  inner ()
