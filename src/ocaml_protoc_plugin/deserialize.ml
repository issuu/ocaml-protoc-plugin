(** Module for deserializing values *)

open StdLabels

module S = Spec.Deserialize
module C = S.C
open S

type 'a sentinal = unit -> 'a
type 'a decoder = Field.t -> 'a

type (_, _) sentinal_list =
  | SNil : ('a, 'a) sentinal_list
  | SCons : ('a sentinal) * ('b, 'c) sentinal_list -> ('a -> 'b, 'c) sentinal_list

let error_wrong_field str field = Result.raise (`Wrong_field_type (str, field))
let error_illegal_value str field = Result.raise (`Illegal_value (str, field))
let error_required_field_missing () = Result.raise `Required_field_missing

let read_varint ~signed ~type_name =
  let open! Infix.Int64 in
  function
  | Field.Varint v -> begin
      let v = match signed with
        | true when v land 0x01L = 0L -> v / 2L
        | true -> (v / 2L * -1L) - 1L
        | false -> v
      in
      v
    end
  | field -> error_wrong_field type_name field

let rec read_varint_unboxed ~signed ~type_name = function
  | Field.Varint_unboxed v -> begin
      let v = match signed with
        | true when v land 0x01 = 0 -> v / 2
        | true -> (v / 2 * -1) - 1
        | false -> v
      in
      v
    end
  | Field.Varint v -> read_varint_unboxed ~signed ~type_name (Field.Varint_unboxed (Int64.to_int v))
  | field -> error_wrong_field type_name field

let read_varint32 ~signed ~type_name field =
  read_varint ~signed ~type_name field |> Int64.to_int32

let rec type_of_spec: type a. a spec -> 'b * a decoder =
  let int_of_int32 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      f field |> Int32.to_int
    in
    (tpe, f)
  in

  let int_of_uint32 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      f field |> (fun v ->
        match Sys.word_size with
        | 32 ->
          (* If the high bit is set, we cannot represent it anyways *)
          Int32.to_int v
        | 64 ->
          let move = 0x1_0000_0000 in
          let i = Int32.to_int v in (if i < 0 then i + move else i)
        | _ -> assert false
      )
    in
    (tpe, f)
  in

  let int_of_int64 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      f field |> Int64.to_int
    in
    (tpe, f)
  in

  let int_of_uint64 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      (* If high-bit is set, we cannot represent it *)
      f field |> Int64.to_int
    in
    (tpe, f)
  in

  function
  | Double -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> Int64.float_of_bits v
      | field -> error_wrong_field "double" field)
  | Float -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> Int32.float_of_bits v
      | field -> error_wrong_field "float" field)
  | Int32 -> (`Varint, read_varint32 ~signed:false ~type_name:"int32")
  | Int32_int -> (`Varint_unboxed, read_varint_unboxed ~signed:false ~type_name:"int32")
  | Int64 ->  (`Varint, read_varint ~signed:false ~type_name:"int64")
  | Int64_int -> (`Varint_unboxed, read_varint_unboxed ~signed:false ~type_name:"int64")
  | UInt32 -> (`Varint, read_varint32 ~signed:false ~type_name:"uint32")
  | UInt32_int -> (`Varint_unboxed, read_varint_unboxed ~signed:false ~type_name:"uint32")
  | UInt64 -> (`Varint, read_varint ~signed:false ~type_name:"uint64")
  | UInt64_int -> (`Varint_unboxed, read_varint_unboxed ~signed:false ~type_name:"uint64")
  | SInt32 -> (`Varint, read_varint32 ~signed:true ~type_name:"sint32")
  | SInt32_int -> (`Varint_unboxed, read_varint_unboxed ~signed:true ~type_name:"sint32")
  | SInt64 -> (`Varint, read_varint ~signed:true ~type_name:"sint64")
  | SInt64_int -> (`Varint_unboxed, read_varint_unboxed ~signed:true ~type_name:"sint64")
  | Fixed32 -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> v
      | field -> error_wrong_field "fixed32" field)
  | Fixed32_int -> int_of_uint32 Fixed32
  | Fixed64 -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> v
      | field -> error_wrong_field "fixed64" field)
  | Fixed64_int -> int_of_uint64 Fixed64
  | SFixed32 -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> v
      | field -> error_wrong_field "sfixed32" field)
  | SFixed32_int -> int_of_int32 SFixed32
  | SFixed64 -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> v
      | field -> error_wrong_field "sfixed64" field)
  | SFixed64_int -> int_of_int64 SFixed64
  | Bool -> (`Varint_unboxed, function
      | Field.Varint_unboxed v -> v != 0
      | Field.Varint v -> Int64.equal v 0L |> not
      | field -> error_wrong_field "bool" field)
  | Enum of_int -> (`Varint_unboxed, function
      | Field.Varint_unboxed v -> of_int v
      | Field.Varint v -> Int64.to_int v |> of_int
      | field -> error_wrong_field "enum" field)
  | String -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> String.sub ~pos:offset ~len:length data
      | field -> error_wrong_field "string" field)
  | Bytes -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> Bytes.sub ~pos:offset ~len:length (Bytes.unsafe_of_string data)
      | field -> error_wrong_field "bytes" field)
  | Message from_proto -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> from_proto (Reader.create ~offset ~length data)
      | field ->  error_wrong_field "message" field)

let default_of_field_type = function
  | `Fixed_32_bit -> Field.fixed_32_bit Int32.zero
  | `Fixed_64_bit -> Field.fixed_64_bit Int64.zero
  | `Length_delimited -> Field.length_delimited ""
  | `Varint -> Field.varint 0L
  | `Varint_unboxed -> Field.varint_unboxed 0

type expect = [ `Fixed_32_bit
              | `Fixed_64_bit
              | `Length_delimited
              | `Varint
              | `Varint_unboxed
              | `Any ]


let get_boxed_type = function
  | `Varint -> Reader.Boxed
  | `Varint_unboxed -> Reader.Unboxed
  | `Fixed_32_bit -> Reader.Boxed
  | `Fixed_64_bit -> Reader.Boxed
  | `Length_delimited -> Reader.Unboxed

let sentinal: type a. a compound -> (int * (unit decoder * Reader.boxed)) list * a sentinal = function
  (* This is the same as required, so we should just use that! *)
  | Basic (index, (Message deser), _) ->
    let v = ref None in
    let get () = match !v with
      | None -> error_required_field_missing ()
      | Some v -> v
    in
    let read = function
      | Field.Length_delimited {offset; length; data} ->
        let reader = Reader.create ~length ~offset data in
        deser reader |> fun message -> v := Some message
      | field -> error_wrong_field "message" field
    in
    ([index, (read, Unboxed)], get)
  | Basic (index, spec, None) ->
    let expect, read = type_of_spec spec in
    let boxed = get_boxed_type expect in
    let v = ref None in
    let get () = match !v with
      | Some v -> v
      | None -> error_required_field_missing ()
    in
    let read field =
      read field |> fun value -> v := Some value
    in
    ([index, (read, boxed)], get)
  | Basic (index, spec, default) ->
    let field_type, read = type_of_spec spec in
    let boxed = get_boxed_type field_type in
    let default = match default with

      | Some default -> default
      | None -> begin
          default_of_field_type field_type
          |> fun v -> try read v with Result.Error _ -> failwith "Cannot decode default field value"
        end
    in
    let v = ref default in
    let get () = !v in
    let read field =
      read field |> fun value -> v := value
    in
    ([index, (read, boxed)], get)
  | Basic_opt (index, spec) ->
    let field_type, read = type_of_spec spec in
    let boxed = get_boxed_type field_type in
    let v = ref None in
    let get () = !v in
    let read field =
      read field |> fun value -> v := Some value
    in
    ([index, (read, boxed)], get)
  | Repeated (index, spec, _) ->
    let read_field = function
      | `Length_delimited -> None
      | `Varint -> Some Reader.read_varint
      | `Varint_unboxed -> Some Reader.read_varint_unboxed
      | `Fixed_64_bit -> Some Reader.read_fixed64
      | `Fixed_32_bit -> Some Reader.read_fixed32
    in
    let rec read_repeated reader decode read_f =
      match Reader.has_more reader with
      | false -> ()
      | true ->
        decode reader |> fun field ->
          read_f field |> fun () ->
            read_repeated reader decode read_f
    in
    let (field_type, read_type) = type_of_spec spec in
    let boxed = get_boxed_type field_type in
    let read_field_type = read_field field_type in
    let v = ref [] in
    let get () = List.rev !v in
    let rec read field = match field, read_field_type with
      | (Field.Length_delimited _ as field), None ->
        read_type field |> fun v' -> v := v' :: !v
      | Field.Length_delimited { offset; length; data }, Some read_field ->
        read_repeated (Reader.create ~offset ~length data) read_field read
      | field, _ -> read_type field |> fun v' -> v := v' :: !v
    in
    ([index, (read, boxed)], get)
  | Oneof oneofs ->
    let make_reader: a ref -> a oneof -> (int * (unit decoder * Reader.boxed)) = fun v (Oneof_elem (index, spec, constr)) ->
      let field_type, read = type_of_spec spec in
      let boxed = get_boxed_type field_type in
      let read field =
        read field |> fun value -> v := (constr value)
      in
      (index, (read, boxed))
    in
    let v = ref `not_set in
    let get () = !v in
    List.map ~f:(make_reader v) oneofs, get

module Map = struct
  include Map.Make (struct type t = int let compare = compare end)
  let of_alist_exn l = List.fold_left ~init:empty ~f:(fun acc (k, v) ->
    if mem k acc then
      invalid_arg "Duplicate keys in list"
    else
      add k v acc
  ) l
end

let in_extension_ranges extension_ranges index =
  List.exists ~f:(fun (start, end') -> index >= start && index <= end') extension_ranges

(** Read fields - map based for nlogn lookup *)
(* The reader list should contain expected type to be read, so we know if it should be unboxed or not *)
let read_fields_map extension_ranges reader_list =
  let extensions = ref [] in
  let map = Map.of_alist_exn reader_list in
  let read_field_content_boxed = Reader.read_field_content Reader.Boxed in
  let rec read reader =
    match Reader.has_more reader with
    | false -> List.rev !extensions
    | true ->
      begin
        let (field_type, field_number) = Reader.read_field_header reader in
        match Map.find_opt field_number map with
        | Some (f, boxed) ->
          let field = Reader.read_field_content boxed field_type reader in
          f field;
          read reader
        | None when in_extension_ranges extension_ranges field_number ->
          (* Dont really know what to set expect to here. It really depends on the options *)
          (* Maybe we should just construct the reader based on boxing or not boxing *)
          (* When is this reading done??? We could just examine the spec string and derive the boxing or unboxing *)
          (* We really need to have this information at the get-go. *)
          let field = read_field_content_boxed field_type reader in
          extensions := (field_number, field) :: !extensions;
          read reader
        | None ->
          let _ = read_field_content_boxed field_type reader in
          read reader
      end
  in
  read

(** Read fields - array based for O(1) lookup *)
let read_fields_array extension_ranges max_index reader_list =
  let extensions = ref [] in
  let default_f index field =
    match in_extension_ranges extension_ranges index with
    | true -> extensions := (index, field) :: !extensions;
      ()
    | false ->
      ()
  in
  let readers = Array.init (max_index + 1) ~f:(fun _ -> Reader.Boxed, default_f) in
  List.iter ~f:(fun (idx, (f, expect)) -> readers.(idx) <- expect, fun _ -> f) reader_list;
  let read_field_content_boxed = Reader.read_field_content Reader.Boxed in

  let rec read reader =
    match Reader.has_more reader with
    | false -> List.rev !extensions
    | true -> begin
        let field_type, field_index = Reader.read_field_header reader in
        match field_index <= max_index with
        | true ->
          let (boxed, f) = readers.(field_index) in
          let field = Reader.read_field_content boxed field_type reader in
          f field_index field;
          read reader
        | false ->
          let field = read_field_content_boxed field_type reader in
          default_f field_index field;
          read reader
      end
  in
  read

let deserialize: type constr t. (int * int) list -> (constr, t) compound_list -> ((int * Field.t) list -> constr) -> Reader.t -> t = fun extension_ranges spec constr ->
  let max_index =
    let rec inner: type a b. int -> (a, b) compound_list -> int = fun acc -> function
      | Cons (Oneof oneofs, rest) ->
        let rec max_elt: type c. int -> c oneof list -> int = fun acc -> function
          | Oneof_elem (idx, _, _) :: rest -> max_elt (max idx acc) rest
          | [] -> acc
        in
        let acc = max_elt acc oneofs in
        inner acc rest
      | Cons (Basic (idx, _, _), rest) ->
        inner (max acc idx) rest
      | Cons (Basic_opt (idx, _), rest) ->
        inner (max acc idx) rest
      | Cons (Repeated (idx, _, _), rest) ->
        inner (max acc idx) rest
      | Nil -> acc
    in
    inner 0 spec
  in
  (* For even better optimization, the first pass could assume that
     all fields are written (if at all) in the same order as the spec.
     If we reach the end of the reader list, we revert to use read_fields_array
     or read_fields_map.

     Even even better, we could read opportunistically, and apply to the constructor
     as soon as we find the element.
     This requires that fields are collected into lists.
     Also Oneof must be in the correct order,
     and requires special handling (But I dont see that as a problem).
     This solution avoids the need of sentinals, and has O(n),
     where n is the number of fields.
     If passing fails, we start over and apply standard readfields.
  *)
  let read_fields = match max_index < 1024 with
    | true -> read_fields_array extension_ranges max_index
    | false -> read_fields_map extension_ranges
  in
  let rec apply: type constr t. constr -> (constr, t) sentinal_list -> t = fun constr -> function
    | SCons (sentinal, rest) ->
      sentinal () |> fun v -> apply (constr v) rest
    | SNil -> constr
  in
  (* We first make a list of sentinal_getters, which we can map to the constr *)
  let rec make_sentinals: type a b. (a, b) compound_list -> (a, b) sentinal_list * (int * (unit decoder * Reader.boxed)) list = function
    | Cons (spec, rest) ->
      let (readers, sentinal) = sentinal spec in
      let (sentinals, reader_list) = make_sentinals rest in
      SCons (sentinal, sentinals), List.rev_append readers reader_list
    | Nil -> SNil, []
  in
  fun reader ->
    let sentinals, reader_list = make_sentinals spec in
    (* Read the fields one by one, and apply the reader - if found *)
    read_fields reader_list reader |> fun extensions -> apply (constr extensions) sentinals
