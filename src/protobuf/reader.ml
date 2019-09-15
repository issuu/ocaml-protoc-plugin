(** Some buffer to hold data, and to read and write data *)

open Core
open Spec
open Result.Let_syntax

type t = {
  mutable offset : int;
  mutable data : Bytes.t;
}

type error = [ `Premature_end_of_input
             | `Unknown_field_type of int
             ] [@@deriving show]

let create data = { data = Bytes.of_string data; offset = 0 }

(** Seems to be two different types here *)
let reset t =
  let data = Bytes.sub ~pos:0 ~len:t.offset t.data in
  { data; offset = 0 }

let contents t = Bytes.sub ~pos:0 ~len:t.offset t.data |> Bytes.to_string

(** Return an error if there is not enough data in input *)
let validate_capacity t count =
  match Bytes.length t.data >= t.offset + count with
  | true -> Result.ok_unit
  | false -> Result.fail `Premature_end_of_input

(** Test if there is more data in the buffer to be read *)
let has_more t =
  t.offset < Bytes.length t.data

let read_byte t =
  let%bind () = validate_capacity t 1 in
  let v = Bytes.get t.data t.offset in
  t.offset <- t.offset + 1;
  return (Char.to_int v)

let read_raw_varint t =
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

let read_varint t =
  read_raw_varint t >>| fun v -> Varint v

let read_field_header : t -> (int * int, error) result = fun t ->
  let%bind v = read_raw_varint t in
  let tpe = v land 0x7 in
  let field_number = v / 8 in
  return (tpe, field_number)

let read_data t =
  let%bind size = read_raw_varint t in
  let%bind () = validate_capacity t size in
  let v = Bytes.sub t.data ~pos:t.offset ~len:size in
  t.offset <- t.offset + size;
  return (Length_delimited (v |> Bytes.to_string))

let read_int32 t =
  let size = 4 in
  let%bind () = validate_capacity t size in
  let v = EndianBytes.LittleEndian.get_int32 t.data t.offset in
  t.offset <- t.offset + size;
  return (Fixed_32_bit v)

let read_int64 t =
  let size = 8 in
  let%bind () = validate_capacity t size in
  let v = EndianBytes.LittleEndian.get_int64 t.data t.offset in
  t.offset <- t.offset + size;
  return (Fixed_64_bit v)

let read_field : t -> (int * field, error) result = fun t ->
  let%bind (field_type, field_number) = read_field_header t in
  let%bind field =
    match field_type with
    | 0 ->
      read_varint t
    | 1 ->
      read_int64 t
    | 2 ->
      read_data t
    | 5 ->
      read_int32 t
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
