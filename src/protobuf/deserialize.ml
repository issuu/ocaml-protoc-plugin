(** Module for deserializing values *)

open Base
open Result.Let_syntax

type error =
  [ Reader.error
  | `Wrong_field_type of string * Spec.field
  | `Illegal_value of string * Spec.field
  | `Not_implemented
  | `Unknown_enum_value of int
  | `Oneof_missing ]
[@@deriving show]

type nonrec 'a result = ('a, error) Result.t

type 'a sentinal = unit -> 'a result
type 'a decoder = Spec.field -> 'a result

type _ spec =
  | Double : float spec
  | Float : float spec
  | Int32 : int spec
  | Int64 : int spec
  | UInt32 : int spec
  | UInt64 : int spec
  | SInt32 : int spec
  | SInt64 : int spec
  | Fixed32 : int spec
  | Fixed64 : int spec
  | SFixed32 : int spec
  | SFixed64 : int spec
  | Bool : bool spec
  | String : string spec
  | Bytes : bytes spec
  | Enum: (int -> 'a result) -> 'a spec
  | Message: (Reader.t -> 'a result) -> 'a spec
  | MessageOpt: (Reader.t -> 'a result) -> 'a option spec

type _ oneof =
  | Oneof_elem : (int * 'b spec * ('b -> 'a)) -> 'a oneof

type _ compound =
  | Basic : int * 'a spec -> 'a compound
  | Repeated : int * 'a spec -> 'a list compound
  | Oneof : 'a oneof list -> 'a compound

type (_, _) compound_list =
  | Nil : ('a, 'a) compound_list
  | Cons : ('a compound) * ('b, 'c) compound_list -> ('a -> 'b, 'c) compound_list

type (_, _) sentinal_list =
  | SNil : ('a, 'a) sentinal_list
  | SCons : ('a sentinal) * ('b, 'c) sentinal_list -> ('a -> 'b, 'c) sentinal_list


let error_wrong_field str field : _ result =
  `Wrong_field_type (str, field) |> Result.fail

let error_illegal_value str field : _ result = `Illegal_value (str, field) |> Result.fail

let read_varint ~signed ~type_name = function
  | Spec.Varint v -> begin
      let v = match signed with
        | true when v % 2 = 0 -> v / 2
        | true -> (v / 2 * -1) - 1
        | false -> v
      in
      return v
    end
  | field -> error_wrong_field type_name field

let ok_exn = function
  | Ok v -> v
  | Error _ -> failwith "Must contain a usable value"

let type_of_spec: type a. a spec -> 'b * a decoder = function
  | Double -> (`Fixed_64_bit, function
      | Spec.Fixed_64_bit v -> return (Int64.float_of_bits v)
      | field -> error_wrong_field "double" field)
  | Float -> (`Fixed_32_bit, function
      | Spec.Fixed_32_bit v -> return (Int32.float_of_bits v)
      | field -> error_wrong_field "float" field)
  | Int32 -> (`Varint, fun field ->
      let%bind v = read_varint ~signed:false ~type_name:"int32" field in
      return (match v lsr 31 with 1 -> v lor 0x7fffffff00000000 | _ -> v)
    )
  | Int64 -> (`Varint, read_varint ~signed:false ~type_name:"int64")
  | UInt32 -> (`Varint, read_varint ~signed:false ~type_name:"uint32")
  | UInt64 -> (`Varint, read_varint ~signed:false ~type_name:"uint64")
  | SInt32 -> (`Varint, read_varint ~signed:true ~type_name:"sint32")
  | SInt64 -> (`Varint, read_varint ~signed:true ~type_name:"sint64")
  | Fixed32 -> (`Fixed_32_bit, function
      | Spec.Fixed_32_bit v -> return (Int32.to_int_exn v)
      | field -> error_wrong_field "fixed32" field)
  | Fixed64 -> (`Fixed_64_bit, function
      | Spec.Fixed_64_bit v -> return (Int64.to_int_exn v)
      | field -> error_wrong_field "fixed64" field)
  | SFixed32 -> (`Fixed_32_bit, function
      | Spec.Fixed_32_bit v -> return (Int32.to_int_exn v)
      | field -> error_wrong_field "sfixed32" field)
  | SFixed64 -> (`Fixed_64_bit, function
      | Spec.Fixed_64_bit v -> return (Int64.to_int_exn v)
      | field -> error_wrong_field "sfixed64" field)
  | Bool -> (`Varint, function
      | Spec.Varint v -> return (v <> 0)
      | field -> error_wrong_field "bool" field)
  | Enum of_int -> (`Varint, function
      | Spec.Varint v -> of_int v
      | field -> error_wrong_field "enum" field)
  | String -> (`Length_delimited, function
      | Spec.Length_delimited {offset; length; data} -> return (String.sub ~pos:offset ~len:length data)
      | field -> error_wrong_field "string" field)
  | Bytes -> (`Length_delimited, function
      | Spec.Length_delimited {offset; length; data} -> return (String.sub ~pos:offset ~len:length data |> Bytes.of_string)
      | field -> error_wrong_field "string" field)
  | Message from_proto -> (`Length_delimited, function
      | Spec.Length_delimited {offset; length; data} -> from_proto (Reader.create ~offset ~length data)
      | field ->  error_wrong_field "message" field)
  | MessageOpt from_proto -> (`Length_delimited, function
      | Spec.Length_delimited {offset; length; data} -> from_proto (Reader.create ~offset ~length data) |> Result.map ~f:Option.some
      | field ->  error_wrong_field "message" field)


let default_of_field_type = function
  | `Fixed_32_bit -> Spec.fixed_32_bit Int32.zero
  | `Fixed_64_bit -> Spec.fixed_64_bit Int64.zero
  | `Length_delimited -> Spec.length_delimited ""
  | `Varint -> Spec.Varint 0

let sentinal: type a. a compound -> (int * unit decoder) list * a sentinal = function
  | Basic (index, (MessageOpt deser)) ->
    let v = ref None in
    let get () = return !v in
    let read = function
      | Spec.Length_delimited {offset; length; data} ->
        let reader = Reader.create ~length ~offset data in
        let%bind message = deser reader in
        v := Some message;
        return ()
      | field -> error_wrong_field "message" field
    in
    ([index, read], get)

  | Basic (index, spec) ->
    let field_type, read = type_of_spec spec in
    let v = ref (read (default_of_field_type field_type) |> function Ok v -> v | Error _ -> failwith "Default value not decodable") in
    let get () = return !v in
    let read field =
      let%bind value = read field in
      v := value;
      return ()
    in
    ([index, read], get)
  | Repeated (index, spec) ->
    let read_field = function
      | `Length_delimited -> None
      | `Varint -> Some Reader.read_varint
      | `Fixed_64_bit -> Some Reader.read_fixed64
      | `Fixed_32_bit -> Some Reader.read_fixed32
    in
    let rec read_repeated reader decode read_f = match Reader.has_more reader with
      | false -> return ()
      | true ->
        let%bind field = decode reader in
        let%bind () = read_f field in
        read_repeated reader decode read_f
    in
    let (field_type, read_type) = type_of_spec spec in
    let v = ref [] in
    let get () = return (List.rev !v) in
    let rec read field = match field, read_field field_type with
      | (Spec.Length_delimited _ as field), None ->
        let%bind v' = read_type field in
        v := v' :: !v;
        return ()
      | Spec.Length_delimited { offset; length; data }, Some read_field ->
        read_repeated (Reader.create ~offset ~length data) read_field read
      | field, _ -> let%bind v' = read_type field in
        v := v' :: !v;
        return ()
    in
    ([index, read], get)
  | Oneof oneofs ->
    let make_reader: a result ref -> a oneof -> (int * unit decoder) = fun v (Oneof_elem (index, spec, constr)) ->
      let _, read = type_of_spec spec in
      let read field =
        let%bind value = read field in
        v := Ok (constr value);
        return ()
      in
      (index, read)
    in
    let v = ref (Error `Oneof_missing) in
    let get () = !v in
    List.map ~f:(make_reader v) oneofs, get

(** Read fields and apply to a map *)
let rec read_fields_map reader map =
  match Reader.has_more reader with
  | false -> return ()
  | true ->
    let%bind (index, field) = match Reader.read_field reader with
      | Ok (index, field) -> return  (index, field)
      | Error err -> Error (err :> error)
    in
    let%bind () = match Map.find map index with
      | Some reader -> reader field
      | None -> return () (* Just ignore *)
    in
    read_fields_map reader map

let deserialize: type constr t. (constr, t) compound_list -> constr -> Reader.t -> t result = fun spec constr reader ->
  let rec apply: type constr t. constr -> (constr, t) sentinal_list -> t result = fun constr -> function
    | SCons (sentinal, rest) ->
      let%bind v = sentinal () in
      apply (constr v) rest
    | SNil -> return constr
  in
  (* We first make a list of sentinal_getters, which we can map to the constr *)
  let rec make_sentinals: type a b. (a, b) compound_list -> (a, b) sentinal_list * (int * unit decoder) list = function
    | Cons (spec, rest) ->
      (* Ouch. Oneofs would return multiple reads and indexes, but only one sentinal *)
      let (readers, sentinal) = sentinal spec in
      (* Fuck. This is getting all backwards. *)
      let (sentinals, reader_list) = make_sentinals rest in
      SCons (sentinal, sentinals), List.rev_append readers reader_list
    | Nil -> SNil, []
  in
  let sentinals, reader_list = make_sentinals spec in
  (* Create a map for nlogn lookup *)
  let map = Map.of_alist_exn (module Int) reader_list in

  (* Read the fields one by one, and apply the reader - if found *)
  let%bind () = read_fields_map reader map in
  apply constr sentinals


(* Idea - We could use a O(n) lookup, where the head is the expected type to read *)
(* If we receive another id, we place the reader in the tail (an accumulator). *)
(* So if we receive everythin in order, its a o(1). Worst case its O(N^2). *)
(* The hard part is to throw away values which has no decoders. *)

(* Another solution is to make an array - If the max id is small enough *)

(* Rather than using sentinals, it would be preferred to update a state.
   But it would need a map, containing different types of values - which is not possible AFAIK.
   We could have an array, again different types are not easy.
*)
(** Module to construct a spec *)
module C = struct
  let double = Double
  let float = Float
  let int32 = Int32
  let int64 = Int64
  let uint32 = UInt32
  let uint64 = UInt64
  let sint32 = SInt32
  let sint64 = SInt64
  let fixed32 = Fixed32
  let fixed64 = Fixed64
  let sfixed32 = SFixed32
  let sfixed64 = SFixed64
  let bool = Bool
  let string = String
  let bytes = Bytes
  let enum f = Enum f
  let message f = Message f
  let messageopt f = MessageOpt f

  let repeated (i, s) = Repeated (i, s)
  let basic (i, s) = Basic (i, s)
  let oneof s = Oneof s
  let oneof_elem s = Oneof_elem s

  let ( ^:: ) a b = Cons (a, b)
  let nil = Nil
end
