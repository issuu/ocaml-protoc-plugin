(** Module for deserializing values *)
open StdLabels

module S = Spec.Deserialize
module C = S.C
open S

(** Exception indicating that fast deserialization did not succeed and revert to full deserialization *)
exception Restart_full

type 'a reader = 'a -> Reader.t -> Field.field_type -> 'a
type ('a, 'b) getter = 'a -> 'b
type 'a field_spec = (int * 'a reader)
type _ value = Value: ('b field_spec list * 'b * ('b, 'a) getter) -> 'a value [@@unboxed]
type extensions = (int * Field.t) list

type (_, _) value_list =
  | VNil : ('a, 'a) value_list
  | VNil_ext : (extensions -> 'a, 'a) value_list
  | VCons : 'a value * ('b, 'c) value_list -> ('a -> 'b, 'c) value_list

type sentinel_field_spec = int * (Reader.t -> Field.field_type -> unit)
type 'a sentinel_getter = unit -> 'a

type (_, _) sentinel_list =
  | NNil : ('a, 'a) sentinel_list
  | NNil_ext: (extensions -> 'a, 'a) sentinel_list
  | NCons : (sentinel_field_spec list  * 'a sentinel_getter) * ('b, 'c) sentinel_list -> ('a -> 'b, 'c) sentinel_list

let error_wrong_field str field = Result.raise (`Wrong_field_type (str, field))
let error_required_field_missing index spec = Result.raise (`Required_field_missing (index, Spec.Deserialize.C.show spec))

let decode_zigzag v =
  let open Infix.Int64 in
  match v land 0x01L = 0L with
  | true -> v / 2L
  | false -> (v / 2L * -1L) - 1L

let decode_zigzag_unboxed v =
  match v land 0x01 = 0 with
  | true -> v / 2
  | false -> (v / 2 * -1) - 1

let int_of_uint32 v =
  let v = Int32.to_int v in
  match Sys.word_size with
  | 32 -> v
  | 64 when v < 0 -> v + 0x1_0000_0000
  | 64 -> v
  | _ -> assert false


let read_of_spec: type a. a spec -> Field.field_type * (Reader.t -> a) = function
  | Double -> Fixed64, fun reader -> Reader.read_fixed64 reader |> Int64.float_of_bits
  | Float  -> Fixed32, fun reader -> Reader.read_fixed32 reader |> Int32.float_of_bits
  | Int32 -> Varint, fun reader -> Reader.read_varint_unboxed reader |> Int32.of_int
  | Int32_int -> Varint, Reader.read_varint_unboxed
  | Int64 -> Varint, Reader.read_varint
  | Int64_int -> Varint, Reader.read_varint_unboxed
  | UInt32 -> Varint, fun reader -> Reader.read_varint_unboxed reader |> Int32.of_int
  | UInt32_int -> Varint, Reader.read_varint_unboxed
  | UInt64 -> Varint, Reader.read_varint
  | UInt64_int -> Varint, Reader.read_varint_unboxed
  | SInt32 -> Varint, fun reader -> Reader.read_varint_unboxed reader |> decode_zigzag_unboxed |> Int32.of_int
  | SInt32_int -> Varint, fun reader -> Reader.read_varint_unboxed reader |> decode_zigzag_unboxed
  | SInt64 -> Varint, fun reader -> Reader.read_varint reader |> decode_zigzag
  | SInt64_int -> Varint, fun reader -> Reader.read_varint_unboxed reader |> decode_zigzag_unboxed

  | Fixed32 -> Fixed32, Reader.read_fixed32
  | Fixed32_int -> Fixed32, fun reader -> Reader.read_fixed32 reader |> int_of_uint32
  | SFixed32 -> Fixed32, Reader.read_fixed32
  | SFixed32_int -> Fixed32, fun reader -> Reader.read_fixed32 reader |> Int32.to_int

  | Fixed64 -> Fixed64, Reader.read_fixed64
  | Fixed64_int -> Fixed64, fun reader -> Reader.read_fixed64 reader |> Int64.to_int
  | SFixed64 -> Fixed64, Reader.read_fixed64
  | SFixed64_int -> Fixed64, fun reader -> Reader.read_fixed64 reader |> Int64.to_int

  | Bool -> Varint, fun reader -> Reader.read_varint_unboxed reader != 0
  | Enum of_int -> Varint, fun reader -> Reader.read_varint_unboxed reader |> of_int
  | String -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    String.sub ~pos:offset ~len:length data
  | Bytes -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    let v = Bytes.create length in
    Bytes.blit_string ~src:data ~src_pos:offset ~dst:v ~dst_pos:0 ~len:length;
    v
  | Message (from_proto, _merge) -> Length_delimited, fun reader ->
    let Field.{ offset; length; data } = Reader.read_length_delimited reader in
    from_proto (Reader.create ~offset ~length data)

let id x = x
let keep_last _ v = v
let merge_opt merge v1 v2 =
  match v1 with
  | None -> Some v2
  | Some v1 -> Some (merge v1 v2)
let keep_last_opt _ v = Some v

let read_field ~read:(expect, read_f) ~map v reader field_type =
  match expect = field_type with
  | true -> read_f reader |> map v
  | false ->
    let field = Reader.read_field_content field_type reader in
    error_wrong_field "Deserialize" field

let value: type a. a compound -> a value = function
  | Basic_req (index, spec) ->
    let read = read_field ~read:(read_of_spec spec) ~map:keep_last_opt in
    let getter = function Some v -> v | None -> error_required_field_missing index spec in
    Value ([(index, read)], None, getter)
  | Basic (index, spec, default) ->
    let read = read_field ~read:(read_of_spec spec) ~map:keep_last in
    Value ([(index, read)], default, id)
  | Basic_opt (index, spec) ->
    let map = match spec with
      | Message (_, merge) -> merge_opt merge
      | _ -> keep_last_opt
    in
    let read = read_field ~read:(read_of_spec spec) ~map in
    Value ([(index, read)], None, id)
  | Repeated (index, spec, Packed) ->
    let field_type, read_f = read_of_spec spec in
    let rec read_packed_values read_f acc reader =
      match Reader.has_more reader with
      | true -> read_packed_values read_f (read_f reader :: acc) reader
      | false -> acc
    in
    let read vs reader = fun (ft : Field.field_type) -> match ft with
      | Field.Length_delimited ->
        let Field.{ offset; length; data } = Reader.read_length_delimited reader in
          let reader = Reader.create ~offset ~length data in
          read_packed_values read_f vs reader
      | ft when ft = field_type ->
        read_f reader :: vs
      | ft ->
        let field = Reader.read_field_content ft reader in
        error_wrong_field "Deserialize" field
    in
    Value ([(index, read)], [], List.rev)
  | Repeated (index, spec, Not_packed) ->
    let read = read_field ~read:(read_of_spec spec) ~map:(fun vs v -> v :: vs) in
    Value ([(index, read)], [], List.rev)
  | Oneof oneofs ->
    let make_reader: a oneof -> a field_spec = fun (Oneof_elem (index, spec, constr)) ->
      let read = read_field ~read:(read_of_spec spec) ~map:(fun _ -> constr) in
      (index, read)
    in
    Value (List.map ~f:make_reader oneofs, `not_set, id)

module IntMap = Map.Make(struct type t = int let compare = Int.compare end)

let rec extension_ranges: type a b. (a, b) compound_list -> extension_ranges = function
  | Nil -> []
  | Nil_ext extension_ranges -> extension_ranges
  | Cons (_, rest) -> extension_ranges rest

let in_extension_ranges extension_ranges index =
  List.exists ~f:(fun (start, end') -> index >= start && index <= end') extension_ranges

let rec make_values: type a b. (a, b) compound_list -> (a, b) value_list = function
  | Nil -> VNil
  | Nil_ext _extension_ranges -> VNil_ext
  | Cons (spec, rest) ->
    let value = value spec in
    let values = make_values rest in
    VCons (value, values)

(** Full (slow) deserialization. *)
let deserialize_full: type constr a. extension_ranges -> (constr, a) value_list -> constr -> Reader.t -> a  = fun extension_ranges values constructor reader ->
  let rec make_sentinel_list: type a b. (a, b) value_list -> (a, b) sentinel_list = function
    | VNil -> NNil
    | VNil_ext -> NNil_ext
    (* Consider optimizing when optional is true *)
    | VCons (Value (fields, default, getter), rest) ->
      let v = ref default in
      let get () = getter !v in
      let fields =
        List.map ~f:(fun (index, read) ->
          let read reader field_type = (v := read !v reader field_type) in
          (index, read)
        ) fields
      in
      NCons ((fields, get), make_sentinel_list rest)
  in

  let rec create_map: type a b. _ IntMap.t -> (a, b) sentinel_list -> _ IntMap.t = fun map -> function
    | NNil -> map
    | NNil_ext -> map
    | NCons ((fields, _), rest) ->
      let map =
        List.fold_left ~init:map ~f:(fun map (index, read)-> IntMap.add index read map) fields
      in
      create_map map rest
  in

  let rec apply: type constr a. extensions -> constr -> (constr, a) sentinel_list -> a = fun extensions constr -> function
    | NNil -> constr
    | NNil_ext -> constr extensions
    | NCons ((_, get), rest) ->
      apply extensions (constr (get ())) rest
  in

  let rec read: (Reader.t -> Field.field_type -> unit) IntMap.t -> extensions -> extensions = fun map extensions ->
    match Reader.has_more reader with
    | false -> List.rev extensions
    | true ->
      let (field_type, field_number) = Reader.read_field_header reader in
      match IntMap.find_opt field_number map with
      | Some read_f ->
        read_f reader field_type;
        read map extensions
      | None when in_extension_ranges extension_ranges field_number ->
        let field = Reader.read_field_content field_type reader in
        read map ((field_number, field) :: extensions)
      | None ->
        let (_: Field.t) = Reader.read_field_content field_type reader in
        read map extensions
  in
  let sentinels = make_sentinel_list values in
  let map = create_map IntMap.empty sentinels in
  let extensions = read map [] in
  apply extensions constructor sentinels

(* Fast deserialization. Assumes all fields in input are sorted. *)
let deserialize_fast: type constr a. extension_ranges -> (constr, a) value_list -> constr -> Reader.t -> a  = fun extension_ranges values constr reader ->

  let rec read_fields: type a. extension_ranges -> Field.field_type -> int -> Reader.t -> extensions -> a -> (int * a reader) list -> ((Field.field_type * int) option * extensions * a) = fun extension_ranges tpe idx reader extensions v -> function
    | ((index, read_f) :: _) as lst when idx = index ->
      let v = read_f v reader tpe in
      begin
        match Reader.next_field_header reader with
        | Some (tpe, idx) -> read_fields extension_ranges tpe idx reader extensions v lst
        | None -> (None, extensions, v)
      end
    | rest when in_extension_ranges extension_ranges idx ->
      let extensions = (idx, Reader.read_field_content tpe reader) :: extensions in
      begin
        match Reader.next_field_header reader with
        | Some (tpe, idx) -> read_fields extension_ranges tpe idx reader extensions v rest
        | None -> (None, extensions, v)
      end
    | _ :: rest -> read_fields extension_ranges tpe idx reader extensions v rest
    | [] -> (Some (tpe, idx), extensions, v)
  in

  let rec apply: type constr a. constr -> extensions -> (constr, a) value_list -> a = fun constr extensions -> function
    | VNil -> constr
    | VNil_ext -> constr (List.rev extensions)
    | VCons (Value (_, default, get), vs) -> apply (constr (get default)) extensions vs
  in

  let rec read_values: type constr a. extension_ranges -> (Field.field_type * int) option -> Reader.t -> constr -> extensions -> (constr, a) value_list -> a = fun extension_ranges next_field reader constr extensions vs ->
    match next_field with
    | None -> apply constr extensions vs
    | Some (tpe, idx) -> match vs with
      | VCons (Value (fields, default, get), vs) ->
        let (next_field, extensions, v) = read_fields extension_ranges tpe idx reader extensions default fields in
        read_values extension_ranges next_field reader (constr (get v)) extensions vs
      | VNil | VNil_ext ->
        (* This implies that there are still fields to be read.
           Revert to full deserialization.
        *)
        raise Restart_full
  in
  let next_field = Reader.next_field_header reader in
  read_values extension_ranges next_field reader constr [] values

let deserialize: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader ->
    let offset = Reader.offset reader in
    try
      deserialize_fast extension_ranges values constr reader
    with (Restart_full | Result.Error `Required_field_missing _ ) ->
      (* Revert to full deserialization *)
      Reader.reset reader offset;
      deserialize_full extension_ranges values constr reader

let deserialize_full: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader -> deserialize_full extension_ranges values constr reader

let deserialize_fast: type constr a. (constr, a) compound_list -> constr -> Reader.t -> a = fun spec constr ->
  let extension_ranges = extension_ranges spec in
  let values = make_values spec in
  fun reader -> deserialize_fast extension_ranges values constr reader
