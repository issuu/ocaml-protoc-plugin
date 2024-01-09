(** Some buffer to hold data, and to read and write data *)
open StdLabels
open Field


let sprintf = Printf.sprintf
let printf = Printf.printf


let length_delimited_size_field_length = 5

type substring = { mutable offset: int; buffer: Bytes.t }

type mode = Balanced | Speed | Space
type t = { mutable data: substring list; mode: mode; block_size:int }

let init ?(mode = Space) ?(block_size = 256) () =
  { data = []; mode; block_size }

let size t =
  let rec inner acc = function
    | [] -> acc
    | { offset; _} :: tl -> inner (offset + acc) tl
  in
  inner 0 t.data

let unused_space t =
  let rec inner = function
    | { offset; buffer } :: xs -> (Bytes.length buffer) - offset + inner xs
    | [] -> 0
  in
  inner t.data

(** Get index of most significant bit. *)
let varint_size_reference v =
  let rec inner acc = function
    | 0 -> acc
    | v -> inner (acc + 1) (v lsr 1) [@@ocaml.unrolled 10]
  in

  match v with
  | v when v < 0 -> 10
  | 0 -> 1
  | v -> (6 + inner 0 v) / 7

let varint_size = function
  | v when v < 0 -> 10
  | v when v < 0x80 -> 1
  | v when v < 0x4000 -> 2
  | v when v < 0x200000 -> 3
  | v when v < 0x10000000 -> 4
  | v when v < 0x800000000 -> 5
  | v when v < 0x40000000000 -> 6
  | v when v < 0x2000000000000 -> 7
  | v when v < 0x100000000000000 -> 8
  | _ -> 9

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

(* Write a field delimited length.
   A delimited field length can be no larger than 2^31.
   This function always write 5 bytes (7*5bits > 31bits).
   This allows the field length to be statically allocated and written later
*)
let write_delimited_field_length_fixed_size buffer ~offset v =
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
  let v = Int64.to_int vl in
  (* Int64.to_int just strips the high bit *)
  match (Int64.shift_right_logical vl 62) = 0L with
  | true ->
    (* Bits 63 or 64 are not set, so write as unboxed *)
    write_varint_unboxed buffer ~offset v
  | false ->
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
    let v = v lsr 7 in
    let offset = match vl < 0L with
      | true ->
        Bytes.set_uint8 buffer offset (v lor 128);
        let offset = offset + 1 in
        Bytes.set_uint8 buffer offset (0x01);
        offset
      | false ->
        Bytes.set_uint8 buffer offset v;
        offset
    in
    offset + 1

(** Reference implementation. Uses a loop which is slower than the manually unrolled version *)
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
      inner ~offset:next_offset rem [@@ocaml.unrolled 10]
  in
  inner ~offset v

(** Reference implementation. Uses a loop which is slower than the manually unrolled version *)
let write_varint_unboxed_reference buffer ~offset v =
  let rec inner ~is_negative ~offset v =
    match v lsr 7 with
    | 0 when is_negative ->
      (* If the value was signed, set bit 63 and 64 *)
      inner ~is_negative:false ~offset (v lor 0xC0)
    | 0 ->
      Bytes.set_uint8 buffer offset v;
      offset + 1
    | rem ->
      let v' = v land 0x7F lor 0x80 in
      Bytes.set_uint8 buffer offset v';
      inner ~is_negative ~offset:(offset + 1) rem
  in
  inner ~is_negative:(v < 0) ~offset v

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
  Bytes.blit_string ~src:src ~src_pos ~dst:buffer ~dst_pos:offset ~len;
  offset + len

let ensure_capacity ~size t =
  match t.data with
    | { offset; buffer } as elem :: _ when Bytes.length buffer - offset >= size -> elem
    | tl ->
      let elem = { offset = 0; buffer = Bytes.create (size + t.block_size) } in
      t.data <- elem :: tl;
      elem

let write_value: size:int -> writer:(Bytes.t -> offset:int -> 'a -> int) -> 'a -> t -> unit =
  fun ~size ~writer v t ->
  let elem = ensure_capacity ~size t in
  let offset = writer elem.buffer ~offset:elem.offset v in
  elem.offset <- offset

let write_naked_field buffer ~offset = function
  | Varint_unboxed v -> write_varint_unboxed buffer ~offset v
  | Varint v -> write_varint buffer ~offset v
  | Fixed_32_bit v -> write_fixed32 buffer ~offset v
  | Fixed_64_bit v -> write_fixed64 buffer ~offset v
  | Length_delimited {offset = src_pos; length; data} ->
    write_length_delimited buffer ~offset ~src:data ~src_pos ~len:length

let add_field t field =
  let size = size_of_field field in
  let elem = ensure_capacity ~size t in
  let offset = write_naked_field elem.buffer ~offset:elem.offset field in
  elem.offset <- offset;
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

let write_length_delimited_value ~write v t =
  let rec size_data_added sentinel acc = function
    | [] -> failwith "End of list reached. This is impossible"
    | x :: _ when x == sentinel -> acc
    | { offset; _ } :: xs -> size_data_added sentinel (offset + acc) xs
  in
  let write_balanced v t =
    let sentinel =
      match t.data with
      | { offset; buffer} as sentinel :: _ when offset + length_delimited_size_field_length <= Bytes.length buffer ->
        sentinel
      | _ ->
        let sentinel = { offset = 0; buffer = Bytes.create length_delimited_size_field_length; } in
        t.data <- sentinel :: t.data;
        sentinel
    in
    let offset = sentinel.offset in
    (* Ensure no writes to the sentinel *)
    sentinel.offset <- Int.max_int;
    let _ = write t v in
    let size = size_data_added sentinel 0 t.data in
    let offset = write_varint_unboxed sentinel.buffer ~offset size in
    sentinel.offset <- offset;
    ()
  in
  let write_speed v t =
    let sentinel = ensure_capacity ~size:length_delimited_size_field_length t in
    let offset = sentinel.offset in
    sentinel.offset <- sentinel.offset + length_delimited_size_field_length;
    let _ = write t v in
    let size = size_data_added sentinel (sentinel.offset - (offset + length_delimited_size_field_length)) t.data in
    let _ = write_delimited_field_length_fixed_size sentinel.buffer ~offset size in
    ()
  in
  let write_space v t =
    let sentinel = ensure_capacity ~size:length_delimited_size_field_length t in
    let offset = sentinel.offset in
    sentinel.offset <- sentinel.offset + length_delimited_size_field_length;
    let _ = write t v in
    let size = size_data_added sentinel (sentinel.offset - (offset + length_delimited_size_field_length)) t.data in
    let offset' = write_varint_unboxed sentinel.buffer ~offset size in
    (* Move data, to avoid holes *)
    let () = match (offset + length_delimited_size_field_length = offset') with
      | true -> ()
      | false ->
        (* Offset points to the first new byte. *)
        (*
        Printf.eprintf "\nHole size: %d. %d, %d, %d\n" n offset offset' sentinel.offset;
           Printf.eprintf "Bytes.blit ~src:sentinel.buffer ~src_pos:%d ~dst:sentinel.buffer ~dst_pos:%d ~len:%d\n" (offset+5) offset' (sentinel.offset - (offset + 5));
        *)
        Bytes.blit ~src:sentinel.buffer ~src_pos:(offset+5) ~dst:sentinel.buffer ~dst_pos:offset' ~len:(sentinel.offset - (offset + 5));
        sentinel.offset <- sentinel.offset - (offset+5-offset');
    in
    ()
  in
  match t.mode with
  | Balanced -> write_balanced v t
  | Speed -> write_speed v t
  | Space -> write_space v t

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
  let values = List.init ~len:64 ~f:(fun idx -> 1L lsl idx) @
               List.init ~len:64 ~f:(fun idx -> (-1L) lsl idx)
  in
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


let%test "varint size unrolled" =
  let open Infix.Int64 in
  let values = List.init ~len:64 ~f:(fun idx -> 1L lsl idx) in
  List.fold_left ~init:true ~f:(fun acc v ->
    List.fold_left ~init:acc ~f:(fun acc v ->
      let size_reference = varint_size_reference (Int64.to_int v) in
      let size = varint_size (Int64.to_int v) in
      match size = size_reference with
      | true -> acc
      | false ->
        Printf.printf "varint_size(0x%Lx/%Ld/%d): %d = %d\n" v v (Int64.to_int v) size size_reference;
        false
    ) [v-2L; v-1L; v; v+1L; v+2L]
  ) values
