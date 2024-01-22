(** Some buffer to hold data, and to read and write data *)
open StdLabels

type t = {
  mutable offset : int;
  end_offset : int;
  data : String.t;
}

let create ?(offset = 0) ?length data =
  let end_offset =
    match length with
    | None -> String.length data
    | Some l -> offset + l
  in
  assert (end_offset >= offset);
  assert (String.length data >= end_offset);
  {offset; end_offset; data}

let reset t offset = t.offset <- offset
let offset { offset; _ } = offset

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
  let v = Bytes.get_uint8 (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + 1;
  v

let read_varint_reference t =
  let open Infix.Int64 in
  let rec inner n acc =
    let v = read_byte t |> Int64.of_int in
    let acc = acc + (v land 0x7fL) lsl n in
    match v land 0x80L = 0x80L with
    | true ->
      (* Still More data *)
      inner (Int.add n 7) acc
    | false -> acc
  in
  inner 0 0L

[@@inline]
let read_varint t =
  let rec inner n acc =
    let v = read_byte t in
    let acc = acc + (v land 0x7f) lsl n in
    match v land 0x80 = 0x80 with
    | true when acc < 0 -> begin
        let accl = Int64.of_int acc in (* If bit63 was set, then bit63 and bit64 are now set *)
        let accl = match read_byte t land 0x01 = 0x01 with
          | true -> accl
          | false -> Int64.logand accl 0x7fffffffffffffffL (* Apparently not a negative number after all *)
        in
        accl
      end
    | true -> inner (n + 7) acc
    | false when acc < 0 -> (* Bit 63 is set, convert into a 64 bit integer, but clear bit64  *)
      Int64.logand 0x7fffffffffffffffL (Int64.of_int acc)
    | false -> Int64.of_int acc

  in
  inner 0 0

[@@inline]
let read_varint_unboxed t =
  let rec inner n acc =
    let v = read_byte t in
    let acc = acc + (v land 0x7f) lsl n in
    match v land 0x80 = 0x80 with
    | true ->
      (* Still More data *)
      inner (n + 7) acc
    | false -> acc
  in
  inner 0 0

(* Implement little endian ourselves *)
let read_fixed32 t =
  let size = 4 in
  validate_capacity t size;
  let v = Bytes.get_int32_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  v

let read_fixed64 t =
  let size = 8 in
  validate_capacity t size;
  let v = Bytes.get_int64_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  v

let read_length_delimited t =
  let length = read_varint_unboxed t in
  validate_capacity t length;
  let v = Field.{ offset = t.offset; length = length; data = t.data } in
  t.offset <- t.offset + length;
  v

let read_field_header: t -> Field.field_type * int = fun t ->
  let v = read_varint_unboxed t in
  let tpe : Field.field_type = match v land 0x7 with
    | 0 -> Varint
    | 1 -> Fixed64
    | 2 -> Length_delimited
    | 5 -> Fixed32
    | _ -> failwith (Printf.sprintf "Illegal field header: 0x%x" v)
  in
  let field_number = v / 8 in
  (tpe, field_number)

let read_field_content: Field.field_type -> t -> Field.t = function
  | Varint -> fun r -> Field.Varint (read_varint r)
  | Fixed64 -> fun r -> Field.Fixed_64_bit (read_fixed64 r)
  | Length_delimited -> fun r -> Length_delimited (read_length_delimited r)
  | Fixed32 -> fun r -> Field.Fixed_32_bit (read_fixed32 r)

let to_list: t -> (int * Field.t) list =
  let read_field t =
    let (tpe, index) = read_field_header t in
    let field = read_field_content tpe t in
    (index, field)
  in
  let rec next t () = match has_more t with
    | true -> Seq.Cons (read_field t, next t)
    | false -> Seq.Nil
  in
  fun t ->
    next t |> List.of_seq


let%expect_test "varint boxed" =
  let values = [-2L; -1L; 0x7FFFFFFFFFFFFFFFL; 0x7FFFFFFFFFFFFFFEL; 0x3FFFFFFFFFFFFFFFL; 0x3FFFFFFFFFFFFFFEL; 0L; 1L] in
  List.iter ~f:(fun v ->
    let buffer =
      let writer = Writer.init () in
      Writer.write_varint_value v writer;
      Writer.contents writer
    in
    Printf.printf "0x%016LxL = 0x%016LxL\n"
      (read_varint_reference (create buffer))
      (read_varint (create buffer));
    ()
  ) values;
  [%expect {|
    0xfffffffffffffffeL = 0xfffffffffffffffeL
    0xffffffffffffffffL = 0xffffffffffffffffL
    0x7fffffffffffffffL = 0x7fffffffffffffffL
    0x7ffffffffffffffeL = 0x7ffffffffffffffeL
    0x3fffffffffffffffL = 0x3fffffffffffffffL
    0x3ffffffffffffffeL = 0x3ffffffffffffffeL
    0x0000000000000000L = 0x0000000000000000L
    0x0000000000000001L = 0x0000000000000001L |}]
