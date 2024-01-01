(** Some buffer to hold data, and to read and write data *)

open StdLabels
open Field

let sprintf = Printf.sprintf
let printf = Printf.printf

(** Bytes allocated at end of any data block to reduce number of allocated blocks *)
let space_overhead = 256

(** Hold multiple short strings in a list *)
type substring = { mutable offset: int; buffer: Bytes.t }

type t = { mutable data: substring list }

let init () = { data = [] }
let size t =
  let rec inner acc = function
    | [] -> acc
    | { offset; _} :: tl -> inner (offset + acc) tl
  in
  inner 0 t.data

(** Get index of most significant bit. *)
let varint_size v =
  let rec inner acc = function
    | 0 -> acc
    | v -> inner (acc + 1) (v lsr 1)
  in
  match v with
  | v when v < 0 -> 10
  | 0 -> 1
  | v -> (6 + inner 0 v) / 7

let rec size_of_field = function
  | Varint v -> varint_size (Int64.to_int v)
  | Varint_unboxed v -> varint_size v
  | Fixed_32_bit _ -> 4
  | Fixed_64_bit _ -> 8
  | Length_delimited {length; _} -> size_of_field (Varint_unboxed length) + length

let write_varint buffer ~offset v =
  let rec inner ~offset v : int =
    let next_offset = offset + 1 in
    let open Infix.Int64 in
    match v lsr 7 with
    | 0L ->
      Bytes.set_uint8 buffer offset (Int64.to_int v);
      next_offset
    | rem ->
      Bytes.set_uint8 buffer offset (Int.logor (Int64.to_int v |> Int.logand 0x7F) 0x80);
      inner ~offset:next_offset rem
  in
  inner ~offset v

let write_varint_unboxed buffer ~offset v =
  let is_negative = v < 0 in
  let rec inner ~offset v : int =
    let next_offset = offset + 1 in
    match v lsr 7 with
    | 0 when is_negative -> (* Emulate 64 bit signed integer *)
      Bytes.set_uint8 buffer offset (v lor 0x80);
      Bytes.set_uint8 buffer next_offset 0x01;
      next_offset + 1
    | 0 ->
      Bytes.set_uint8 buffer offset v;
      next_offset
    | rem ->
      let v' = v land 0x7F lor 0x80 in
      Bytes.set_uint8 buffer offset v';
      inner ~offset:next_offset rem
  in
  inner ~offset v

let write_fixed32 buffer ~offset v =
  Bytes.set_int32_le buffer offset v;
  offset + 4

let write_fixed64 buffer ~offset v =
  Bytes.set_int64_le buffer offset v;
  offset + 8

let write_length_delimited buffer ~offset ~src ~src_pos ~len =
  let offset = write_varint_unboxed buffer ~offset len in
  Bytes.blit ~src:(Bytes.of_string src) ~src_pos ~dst:buffer ~dst_pos:offset ~len;
  offset + len

let write_naked_field buffer ~offset = function
  | Varint_unboxed v -> write_varint_unboxed buffer ~offset v
  | Varint v -> write_varint buffer ~offset v
  | Fixed_32_bit v -> write_fixed32 buffer ~offset v
  | Fixed_64_bit v -> write_fixed64 buffer ~offset v
  | Length_delimited {offset = src_pos; length; data} ->
    write_length_delimited buffer ~offset ~src:data ~src_pos ~len:length

let add_field t field =
  let size = size_of_field field in
  let elem, tl = match t.data with
    | { offset; buffer } as elem :: tl when Bytes.length buffer - offset >= size -> elem, tl
    | tl -> { offset = 0; buffer = Bytes.create (size + space_overhead) }, tl
  in
  (* Write *)
  let offset = write_naked_field elem.buffer ~offset:elem.offset field in
  elem.offset <- offset;
  t.data <- elem :: tl;
  ()

let write_field_header : t -> int -> int -> unit =
  fun t index field_type ->
  let header = (index lsl 3) + field_type in
  add_field t (Varint_unboxed (header))

let write_field : t -> int -> Field.t -> unit =
 fun t index field ->
  let field_type =
    match field with
    | Varint _ -> 0
    | Varint_unboxed _ -> 0
    | Fixed_64_bit _ -> 1
    | Length_delimited _ -> 2
    | Fixed_32_bit _ -> 5
  in
  write_field_header t index field_type;
  add_field t field

let add_length_delimited_field_header t index =
  let sentinel = { offset = 0; buffer = Bytes.create 20; } in
  t.data <- sentinel :: t.data;
  write_field_header t index 2;
  let offset = sentinel.offset in
  sentinel.offset <- 20; (* Make sure nothing is written to this again *)
  let rec size_data_added acc = function
    | [] -> failwith "End of list reached. This is impossible"
    | x :: _ when x == sentinel -> acc
    | { offset; _ } :: xs -> size_data_added (offset + acc) xs
  in
  (* Return a function to use when done *)
  fun () ->
    let size = size_data_added 0 t.data in
    let offset = write_naked_field sentinel.buffer ~offset (Varint_unboxed size) in
    sentinel.offset <- offset;
    ()

let contents t =
  let size = size t in
  let contents = Bytes.create size in
  let rec inner offset = function
    | [] -> offset
    | { offset = o; buffer} :: tl ->
       let next_offset = offset - o in
       Bytes.blit ~src:buffer ~src_pos:0 ~dst:contents ~dst_pos:next_offset ~len:o;
       inner (next_offset) tl
  in
  let offset = inner size t.data in
  assert (offset = 0);
  Bytes.unsafe_to_string contents

let dump t =
  let string_contents = contents t in
  List.init ~len:(String.length string_contents) ~f:(fun i ->
    sprintf "%02x" (Char.code (String.get string_contents i))
  )
  |> String.concat ~sep:"-"
  |> printf "Buffer: %s\n"

let of_list: (int * Field.t) list -> t = fun fields ->
  let t = init () in
  List.iter ~f:(fun (index, field) -> write_field t index field) fields;
  t

let%expect_test "Writefield" =
  let buffer = init () in
  write_field buffer 1 (Varint 3L);
  write_field buffer 2 (Varint 5L);
  write_field buffer 3 (Varint 7L);
  write_field buffer 4 (Varint 11L);

  dump buffer;
  [%expect {| Buffer: 08-03-10-05-18-07-20-0b |}]
