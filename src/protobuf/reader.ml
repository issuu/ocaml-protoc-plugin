(** Some buffer to hold data, and to read and write data *)

open StdLabels
open Field
open Result

type t = {
  mutable offset : int;
  end_offset : int;
  data : String.t;
}

type error =
  [ `Premature_end_of_input
  | `Unknown_field_type of int ]
[@@deriving show]

let create ?(offset = 0) ?length data =
  let end_offset =
    match length with
    | None -> String.length data
    | Some l -> l + offset
  in
  assert (String.length data >= end_offset);
  {offset; end_offset; data}

let size {offset; end_offset; _} = end_offset - offset

(** Return an error if there is not enough data in input *)
let validate_capacity t count =
  match t.offset + count <= t.end_offset with
  | true -> return ()
  | false ->
    Result.fail `Premature_end_of_input

(** Test if there is more data in the buffer to be read *)
let has_more t = t.offset < t.end_offset

let read_byte t =
  validate_capacity t 1 >>| fun () ->
    let v = t.data.[t.offset] in
    t.offset <- t.offset + 1;
    (Char.code v)

let read_raw_varint t =
  let open Infix.Int64 in
  let rec inner acc =
    read_byte t >>= fun v ->
      let v = Int64.of_int v in
      let acc = (v land 0x7FL) :: acc in
      match v > 127L with
      | true ->
        (* Still More data *)
        inner acc
      | false -> Result.return acc
  in
  inner [] >>|
  List.fold_left ~init:0L ~f:(fun acc c -> (acc lsl 7) + c)

let read_varint t = read_raw_varint t >>| fun v -> Varint v

let read_field_header : t -> (int * int) Result.t =
  fun t ->
  let open Infix.Int64 in
  read_raw_varint t >>| fun v ->
    let tpe = v land 0x7L |> Int64.to_int in
    let field_number = v / 8L |> Int64.to_int in
    (tpe, field_number)


let read_length_delimited t =
  read_raw_varint t >>= fun length ->
    let length = Int64.to_int length in
    validate_capacity t length >>| fun () ->
      let v = Length_delimited {offset = t.offset; length; data = t.data} in
      t.offset <- t.offset + length;
      v

(* Implement little endian ourselves *)
let read_fixed32 t =
  let size = 4 in
  validate_capacity t size >>| fun () ->
  let v = LittleEndian.get_int32 t.data t.offset in
  t.offset <- t.offset + size;
  (Fixed_32_bit v)

let read_fixed64 t =
  let size = 8 in
  validate_capacity t size >>| fun () ->
  let v = LittleEndian.get_int64 t.data t.offset in
  t.offset <- t.offset + size;
  (Fixed_64_bit v)

let read_field : t -> (int * Field.t) Result.t =
 fun t ->
  read_field_header t >>= (fun (field_type, field_number) ->
    (match field_type with
    | 0 -> read_varint t
    | 1 -> read_fixed64 t
    | 2 -> read_length_delimited t
    | 5 -> read_fixed32 t
    | n -> Result.fail (`Unknown_field_type n))
    >>| fun field -> (field_number, field)
  )
