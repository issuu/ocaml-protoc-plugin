(** Some buffer to hold data, and to read and write data *)
open StdLabels
open Field


let sprintf = Printf.sprintf
let printf = Printf.printf

(** Bytes allocated at end of any data block to reduce number of allocated blocks *)
let space_overhead = 512

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

let unused t =
  let rec inner = function
    | { offset; buffer } :: xs -> (Bytes.length buffer) - offset + inner xs
    | [] -> 0
  in
  inner t.data

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

(* Manually unroll *)
let write_varint_unboxed buffer ~offset = function
  | v when v < 0 ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset 0x01;
    offset + 1

  | v when v < 1 lsl (7*1) ->
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*2) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*3) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*4) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*5) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*6) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (7*7) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v when v < 1 lsl (8*7) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

  | v (* when v < 1 lsl (8*8) *) ->
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset v;
    offset + 1

(* If we clear the top bit, then its not signed anymore... Maybe. *)
let write_varint buffer ~offset vl =
  match Infix.Int64.(vl lsr 62 > 0L) with
  | false -> write_varint_unboxed buffer ~offset (Int64.to_int vl)
  | true ->
    let v = Int64.to_int vl in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let v = v lsr 7 in
    let offset = offset + 1 in
    Bytes.set_uint8 buffer offset (v lor 128);
    let offset = offset + 1 in
    let offset = match vl < 0L with
      | true ->
        Bytes.set_uint8 buffer offset ((Int64.shift_right vl (8*7) |> Int64.to_int) lor 128);
        let offset = offset + 1 in
        Bytes.set_uint8 buffer offset 0x01;
        offset
      | false ->
        Bytes.set_uint8 buffer offset (Int64.shift_right vl (8*7) |> Int64.to_int);
        offset
    in
    offset + 1

let write_varint_reference buffer ~offset v =
  let rec inner ~offset v =
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

let write_varint_unboxed_reference buffer ~offset v =
  let is_negative = v < 0 in
  let v = v land 0x7FFFFFFFFFFFFFFF in
  let rec inner ~offset v : int =
    let next_offset = offset + 1 in
    match v lsr 7 with
    (* This is wrong. We need to know if we should clear bit 63 - and we can do that immediatly *)
    | 0 when is_negative -> (* Emulate 64 bit signed integer *)
      Bytes.set_uint8 buffer offset (v lor 128);
      Bytes.set_uint8 buffer next_offset 0x01; (* Setting the high bit (bit number 64 = 7*9+1 *)
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

let write_string buffer ~offset v =
  let len = String.length v in
  Bytes.blit_string ~src:v ~src_pos:0 ~dst:buffer ~dst_pos:offset ~len;
  offset + len

let write_length_delimited buffer ~offset ~src ~src_pos ~len =
  let offset = write_varint_unboxed buffer ~offset len in
  Bytes.blit ~src:(Bytes.of_string src) ~src_pos ~dst:buffer ~dst_pos:offset ~len;
  offset + len

let write_value: size:int -> writer:(Bytes.t -> offset:int -> 'a -> int) -> 'a -> t -> unit =
  fun ~size ~writer v t ->
  let elem, tl = match t.data with
    | { offset; buffer } as elem :: tl when Bytes.length buffer - offset >= size -> elem, tl
    | tl -> { offset = 0; buffer = Bytes.create (size + space_overhead) }, tl
  in
  let offset = writer elem.buffer ~offset:elem.offset v in
  elem.offset <- offset;
  t.data <- elem :: tl


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

let write_length_delimited_value ~write v writer =
  let rec size_data_added sentinel acc = function
    | [] -> failwith "End of list reached. This is impossible"
    | x :: _ when x == sentinel -> acc
    | { offset; _ } :: xs -> size_data_added sentinel (offset + acc) xs
  in
  let sentinel = match writer.data with
    | { offset; buffer} as sentinel :: _ when offset + 10 <= Bytes.length buffer ->
      sentinel
    | _ ->
      let sentinel = { offset = 0; buffer = Bytes.create 10; } in
      writer.data <- sentinel :: writer.data;
      sentinel
  in
  let offset = sentinel.offset in
  (* Make sure nothing else is written to the sentinel *)
  sentinel.offset <- Int.max_int;
  write v writer;
  let size = size_data_added sentinel 0 writer.data in
  let offset = write_naked_field sentinel.buffer ~offset (Varint_unboxed size) in
  sentinel.offset <- offset

let _add_length_delimited_field_header t index =
  let sentinel = { offset = 0; buffer = Bytes.create 20; } in
  t.data <- sentinel :: t.data;
  write_field_header t index 2; (* Length delimited *)
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

let%test "varint unrolled" =
  let open Infix.Int64 in
  let values = List.init ~len:64 ~f:(fun idx -> 1L lsl idx) in
  List.fold_left ~init:true ~f:(fun acc v ->
    List.fold_left ~init:acc ~f:(fun acc v ->

      let acc =
        let b1 = Bytes.make 10 '\000' in
        let b2 = Bytes.make 10 '\000' in
        write_varint_unboxed_reference b1 ~offset:0 (Int64.to_int v) |> ignore;
        write_varint_unboxed b2 ~offset:0 (Int64.to_int v) |> ignore;
        match Bytes.equal b1 b2 with
        | true -> acc
        | false ->
        Printf.printf "Unboxed: %16Lx (%20d): %S = %S\n" v (Int64.to_int v) (Bytes.to_string b1) (Bytes.to_string b2);
        false
      in
      let acc =
        let b1 = Bytes.make 10 '\000' in
        let b2 = Bytes.make 10 '\000' in
        write_varint_reference b1 ~offset:0 v |> ignore;
        write_varint b2 ~offset:0 v |> ignore;
        match Bytes.equal b1 b2 with
        | true -> acc
        | false ->
        Printf.printf "Boxed: %16Lx: %S = %S\n" v (Bytes.to_string b1) (Bytes.to_string b2);
        false
      in
      acc

    ) [v-2L; v-1L; v; v+1L; v+2L]
  ) values
