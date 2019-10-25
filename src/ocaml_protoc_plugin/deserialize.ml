(** Module for deserializing values *)

open StdLabels
open Result

module S = Spec.Deserialize
module C = S.C
open S

type 'a sentinal = unit -> 'a Result.t
type 'a decoder = Field.t -> 'a Result.t

type (_, _) sentinal_list =
  | SNil : ('a, 'a) sentinal_list
  | SCons : ('a sentinal) * ('b, 'c) sentinal_list -> ('a -> 'b, 'c) sentinal_list

let error_wrong_field str field : _ Result.t =
  `Wrong_field_type (str, field) |> Result.fail

let error_illegal_value str field : _ Result.t = `Illegal_value (str, field) |> Result.fail
let error_required_field_missing: _ Result.t = `Required_field_missing |> Result.fail

let read_varint ~signed ~type_name =
  let open! Infix.Int64 in
  function
  | Field.Varint v -> begin
      let v = match signed with
        | true when v land 0x01L = 0L -> v / 2L
        | true -> (v / 2L * -1L) - 1L
        | false -> v
      in
      return v
    end
  | field -> error_wrong_field type_name field

let read_varint32 ~signed ~type_name field =
  read_varint ~signed ~type_name field >>| Int64.to_int32

let rec type_of_spec: type a. a spec -> 'b * a decoder =
  let int_of_int32 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      f field >>| Int32.to_int
    in
    (tpe, f)
  in

  let int_of_int64 spec =
    let (tpe, f) = type_of_spec spec in
    let f field =
      f field >>| Int64.to_int
    in
    (tpe, f)
  in
  function
  | Double -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> return (Int64.float_of_bits v)
      | field -> error_wrong_field "double" field)
  | Float -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> return (Int32.float_of_bits v)
      | field -> error_wrong_field "float" field)
  | Int32 -> (`Varint, read_varint32 ~signed:false ~type_name:"int32")
  | Int32_int -> int_of_int32 Int32
  | Int64 ->  (`Varint, read_varint ~signed:false ~type_name:"int64")
  | Int64_int -> int_of_int64 Int64
  | UInt32 -> (`Varint, read_varint32 ~signed:false ~type_name:"uint32")
  | UInt32_int -> int_of_int32 UInt32
  | UInt64 -> (`Varint, read_varint ~signed:false ~type_name:"uint64")
  | UInt64_int -> int_of_int64 UInt64
  | SInt32 -> (`Varint, read_varint32 ~signed:true ~type_name:"sint32")
  | SInt32_int -> int_of_int32 SInt32
  | SInt64 -> (`Varint, read_varint ~signed:true ~type_name:"sint64")
  | SInt64_int -> int_of_int64 SInt64
  | Fixed32 -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> return (v)
      | field -> error_wrong_field "fixed32" field)
  | Fixed32_int -> int_of_int32 Fixed32
  | Fixed64 -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> return v
      | field -> error_wrong_field "fixed64" field)
  | Fixed64_int -> int_of_int64 Fixed64

  | SFixed32 -> (`Fixed_32_bit, function
      | Field.Fixed_32_bit v -> return v
      | field -> error_wrong_field "sfixed32" field)
  | SFixed32_int -> int_of_int32 SFixed32
  | SFixed64 -> (`Fixed_64_bit, function
      | Field.Fixed_64_bit v -> return v
      | field -> error_wrong_field "sfixed64" field)
  | SFixed64_int -> int_of_int64 SFixed64
  | Bool -> (`Varint, function
      | Field.Varint v -> return (Int64.equal v 0L |> not)
      | field -> error_wrong_field "bool" field)
  | Enum of_int -> (`Varint, function
      | Field.Varint v -> of_int (Int64.to_int v)
      | field -> error_wrong_field "enum" field)
  | String -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> return (String.sub ~pos:offset ~len:length data)
      | field -> error_wrong_field "string" field)
  | Bytes -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> return (String.sub ~pos:offset ~len:length data |> Bytes.of_string)
      | field -> error_wrong_field "string" field)
  | Message from_proto -> (`Length_delimited, function
      | Field.Length_delimited {offset; length; data} -> from_proto (Reader.create ~offset ~length data)
      | field ->  error_wrong_field "message" field)

let default_of_field_type = function
  | `Fixed_32_bit -> Field.fixed_32_bit Int32.zero
  | `Fixed_64_bit -> Field.fixed_64_bit Int64.zero
  | `Length_delimited -> Field.length_delimited ""
  | `Varint -> Field.varint 0L

let sentinal: type a. a compound -> (int * unit decoder) list * a sentinal = function
  (* This is the same as required, so we should just use that! *)
  | Basic (index, (Message deser), _) ->
    let v = ref None in
    let get () = match !v with
      | None -> error_required_field_missing
      | Some v -> return v
    in
    let read = function
      | Field.Length_delimited {offset; length; data} ->
        let reader = Reader.create ~length ~offset data in
        deser reader >>| fun message -> v := Some message
      | field -> error_wrong_field "message" field
    in
    ([index, read], get)
  | Basic (index, spec, Required) ->
    let _, read = type_of_spec spec in
    let v = ref None in
    let get () = match !v with
      | Some v -> return v
      | None -> error_required_field_missing
    in
    let read field =
      read field >>| fun value -> v := Some value
    in
    ([index, read], get)
  | Basic (index, spec, default) ->
    let field_type, read = type_of_spec spec in
    let default = match default with
      | Proto2 default -> default
      | Required
      | Proto3 -> begin
          default_of_field_type field_type
          |> read
          |> function
          | Ok v -> v
          | Error _ -> failwith "Cannot decode default field value"
        end
    in
    let v = ref default in
    let get () = return !v in
    let read field =
      read field >>| fun value -> v := value
    in
    ([index, read], get)
  | Basic_opt (index, spec) ->
    let _, read = type_of_spec spec in
    let v = ref None in
    let get () = return !v in
    let read field =
      read field >>| fun value -> v := Some value
    in
    ([index, read], get)
  | Repeated (index, spec, _) ->
    let read_field = function
      | `Length_delimited -> None
      | `Varint -> Some Reader.read_varint
      | `Fixed_64_bit -> Some Reader.read_fixed64
      | `Fixed_32_bit -> Some Reader.read_fixed32
    in
    let rec read_repeated reader decode read_f = match Reader.has_more reader with
      | false -> return ()
      | true ->
        decode reader >>= fun field ->
          read_f field >>= fun () ->
            read_repeated reader decode read_f
    in
    let (field_type, read_type) = type_of_spec spec in
    let v = ref [] in
    let get () = return (List.rev !v) in
    let rec read field = match field, read_field field_type with
      | (Field.Length_delimited _ as field), None ->
        read_type field >>| fun v' -> v := v' :: !v
      | Field.Length_delimited { offset; length; data }, Some read_field ->
        read_repeated (Reader.create ~offset ~length data) read_field read
      | field, _ -> read_type field >>| fun v' -> v := v' :: !v
    in
    ([index, read], get)
  | Oneof oneofs ->
    let make_reader: a Result.t ref -> a oneof -> (int * unit decoder) = fun v (Oneof_elem (index, spec, constr)) ->
      let _, read = type_of_spec spec in
      let read field =
        read field >>| fun value -> v := Ok (constr value)
      in
      (index, read)
    in
    let v = ref (Error `Oneof_missing) in
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

(** Read fields - map based for nlogn lookup *)
let read_fields_map reader_list =
  let map = Map.of_alist_exn reader_list in
  let rec read reader =
    match Reader.has_more reader with
    | false -> return ()
    | true -> begin
        match Reader.read_field reader with
        | Ok (index, field) -> begin
            match Map.find_opt index map with
            | Some f ->
              f field >>= fun () ->
              read reader
            | None ->
              read reader
          end
        | Error err -> Error err
      end
  in
  read

(** Read fields - array based for O(1) lookup *)
let read_fields_array max_index reader_list =
  let default _ = Ok () in
  let readers = Array.init (max_index + 1) ~f:(fun _ -> default) in
  List.iter ~f:(fun (idx, f) -> readers.(idx) <- f) reader_list;

  let rec read reader =
    match Reader.has_more reader with
    | false -> return ()
    | true -> begin
        match Reader.read_field reader with
        | Ok (index, field) when index <= max_index ->
          readers.(index) field >>= fun () ->
          read reader
        | Ok _ -> return ()
        | Error err -> Error err
      end
  in
  read

let deserialize: type constr t. (int * int) list -> (constr, t) compound_list -> ((int * Field.t) list -> constr) -> Reader.t -> t Result.t = fun _extension_ranges spec constr ->
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
    | true -> read_fields_array max_index
    | false -> read_fields_map
  in
  let rec apply: type constr t. constr -> (constr, t) sentinal_list -> t Result.t = fun constr -> function
    | SCons (sentinal, rest) ->
      sentinal () >>= fun v -> apply (constr v) rest
    | SNil -> return constr
  in
  (* We first make a list of sentinal_getters, which we can map to the constr *)
  let rec make_sentinals: type a b. (a, b) compound_list -> (a, b) sentinal_list * (int * unit decoder) list = function
    | Cons (spec, rest) ->
      let (readers, sentinal) = sentinal spec in
      let (sentinals, reader_list) = make_sentinals rest in
      SCons (sentinal, sentinals), List.rev_append readers reader_list
    | Nil -> SNil, []
  in
  fun reader ->
    let sentinals, reader_list = make_sentinals spec in
    let constr = constr [] in
    (* Read the fields one by one, and apply the reader - if found *)
    read_fields reader_list reader >>= fun () -> apply constr sentinals
