(** Some buffer to hold data, and to read and write data *)

open StdLabels
open Field

let sprintf = Printf.sprintf
let printf = Printf.printf

type field_list =
  | Nil
  | Cons_field of (Field.t * field_list)
  | Cons_fields of (field_list * field_list)

type t = {
  mutable fields : field_list;
  mutable size: int;
}

(** Yes. But it would be nicer to just iter over them *)
let rev_fields fields =
  let rec inner acc = function
    | Nil -> acc
    | Cons_field (hd, tl) ->
      inner (hd :: acc) tl
    | Cons_fields (hd, tl) ->
      inner (inner acc hd) tl
  in
  inner [] fields

let init () = {fields = Nil; size = 0;}

let rec size_of_field = function
  | Varint v when v > 0L ->
    let bits = int_of_float (log (Int64.to_float v) /. log 2.0) + 1 in
    ((bits - 1) / 7) + 1
  | Varint v when v < 0L -> 10
  | Varint _ -> 1
  | Fixed_32_bit _ -> 4
  | Fixed_64_bit _ -> 8
  | Length_delimited {length; _} -> size_of_field (Varint (Int64.of_int length)) + length

let size t = t.size

let write_varint buffer ~offset v =
  let rec inner ~offset v : int =
    let open Infix.Int64 in
    match v land 0x7FL, v lsr 7 with
    | v, 0L ->
      Bytes.set buffer offset (v |> Int64.to_int |> Char.chr);
      Pervasives.(offset + 1)
    | v, rem ->
      Bytes.set buffer offset (v lor 0x80L |> Int64.to_int |> Char.chr);
      inner ~offset:Pervasives.(offset + 1) rem
  in
  inner ~offset v

let write_fixed32 buffer ~offset v =
  LittleEndian.set_int32 buffer offset v;
  offset + 4

let write_fixed64 buffer ~offset v =
  LittleEndian.set_int64 buffer offset v;
  offset + 8

let write_length_delimited buffer ~offset ~src ~src_pos ~len =
  let offset = write_varint buffer ~offset (Int64.of_int len) in
  Bytes.blit ~src:(Bytes.of_string src) ~src_pos ~dst:buffer ~dst_pos:offset ~len;
  offset + len

let write_field buffer ~offset = function
  | Varint v -> write_varint buffer ~offset v
  | Fixed_32_bit v -> write_fixed32 buffer ~offset v
  | Fixed_64_bit v -> write_fixed64 buffer ~offset v
  | Length_delimited {offset = src_pos; length; data} ->
    write_length_delimited buffer ~offset ~src:data ~src_pos ~len:length

let contents t =
  let size = size t in
  let t = rev_fields t.fields in
  let buffer = Bytes.create size in
  let next_offset =
    List.fold_left ~init:0 ~f:(fun offset field -> write_field buffer ~offset field) t
  in
  assert (next_offset = size);
  Bytes.to_string buffer

let add_field t field =
  t.fields <- Cons_field(field, t.fields);
  t.size <- t.size + size_of_field field;
  ()

(** Add the contents of src as is *)
let concat t ~src =
  t.fields <- Cons_fields(src.fields, t.fields);
  t.size <- t.size + src.size;
  ()

let write_field_header : t -> int -> int -> unit =
  fun t index field_type ->
  let header = (index lsl 3) + field_type in
  add_field t (Varint (Int64.of_int header))

let write_field : t -> int -> Field.t -> unit =
 fun t index field ->
  let field_type =
    match field with
    | Varint _ -> 0
    | Fixed_64_bit _ -> 1
    | Length_delimited _ -> 2
    | Fixed_32_bit _ -> 5
  in
  write_field_header t index field_type;
  add_field t field

(** Add the contents of src as a length_delimited field *)
let concat_as_length_delimited t ~src index =
  let size = size src in
  write_field_header t index 2;
  add_field t (Varint (Int64.of_int size));
  concat t ~src

let dump t =
  let string_contents = contents t in
  List.init ~len:(String.length string_contents) ~f:(fun i ->
    sprintf "%02x" (Char.code (String.get string_contents i))
  )
  |> String.concat ~sep:"-"
  |> printf "Buffer: %s\n"

module Test = struct
  let test () =
    let (_:bool) =
      let buffer = init () in
      write_field buffer 1 (Varint 1L);
      let c = contents buffer in
      String.length c = 2 && String.equal c "\x08\x01" || failwith "Writefield failed"
    in
    ()
end
