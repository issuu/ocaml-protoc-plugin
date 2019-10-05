open Base
open Spec

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
  | Enum: ('a -> int) -> 'a spec
  | Message: ('a -> Writer.t) -> 'a spec
  | MessageOpt: ('a -> Writer.t) -> 'a option spec

type _ oneof_elem =
  | Oneof_elem : (int * 'b spec * 'b) -> unit oneof_elem

type _ compound =
  | Repeated : (int * 'a spec) -> 'a list compound
  | Basic : (int * 'a spec) -> 'a compound
  | Oneof : ('a -> unit oneof_elem) -> 'a compound

type (_, _) compound_list =
  | Nil : ('a, 'a) compound_list
  | Cons : 'a compound * ('b, 'c) compound_list -> ('a -> 'b, 'c) compound_list

let ( ^:: ) a b = Cons (a, b)

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
 fun fields ->
  let writer = Writer.init () in
  List.iter ~f:(fun (index, field) -> Writer.write_field writer index field) fields;
  Writer.contents writer

let unsigned_varint v = Varint v

let signed_varint v =
  let v =
    match v with
    | v when v < 0 -> (((v * -1) - 1) * 2) + 1
    | v -> v * 2
  in
  Varint v

let (=) = Poly.(=)
let%test _ = signed_varint 0 = Varint 0
let%test _ = signed_varint (-1) = Varint 1
let%test _ = signed_varint 1 = Varint 2
let%test _ = signed_varint (-2) = Varint 3
let%test _ = signed_varint 2147483647 = Varint 4294967294
let%test _ = signed_varint (-2147483648) = Varint 4294967295

let rec field_of_spec: type a. a spec -> a -> Spec.field = function
  | Double -> fun v -> Fixed_64_bit (Int64.bits_of_float v)
  | Float -> fun v -> Fixed_32_bit (Int32.bits_of_float v)
  | Int64 -> unsigned_varint
  | UInt64 -> unsigned_varint
  | SInt64 -> signed_varint
  | Int32 -> fun v -> unsigned_varint (v land 0xffffffff)
  | UInt32 -> unsigned_varint
  | SInt32 -> signed_varint
  | Fixed32 -> fun v -> Fixed_32_bit (Int32.of_int_exn v)
  | Fixed64 -> fun v -> Fixed_64_bit (Int64.of_int_exn v)
  | SFixed32 -> fun v -> Fixed_32_bit (Int32.of_int_exn v)
  | SFixed64 -> fun v -> Fixed_64_bit (Int64.of_int_exn v)
  | Bool -> fun v -> unsigned_varint (match v with | true -> 1 | false -> 0)
  | String -> fun v -> Length_delimited {offset = 0; length = String.length v; data = v}
  | Bytes -> fun v -> Length_delimited {offset = 0; length = Bytes.length v; data = Bytes.to_string v}
  | Enum f ->
    let to_field = field_of_spec UInt64 in
    fun v -> f v |> to_field
  | Message to_proto ->
    fun v ->
      let writer = to_proto v in
      Spec.length_delimited (Writer.contents writer)
  | MessageOpt _ -> failwith "Cannot comply"

let is_scalar: type a. a spec -> bool = function
  | String -> false
  | Bytes -> false
  | Message _ -> false
  | MessageOpt _ -> false
  | _ -> true

let rec write: type a. Writer.t -> a compound -> a -> unit = fun writer ->
  function
  | Basic (index, MessageOpt (to_proto)) -> begin
      function
      | Some v ->
        let v = to_proto v in
        Writer.concat_as_length_delimited writer ~src:v index
      | None -> ()
    end
  | Basic (index, Message (to_proto)) ->
    fun v ->
      let v = to_proto v in
      Writer.concat_as_length_delimited writer ~src:v index
  | Repeated (index, Message to_proto) ->
    fun vs -> List.iter ~f:(fun v -> write writer (Basic (index, Message to_proto)) v) vs
  | Repeated (index, spec) when is_scalar spec -> begin
      let f = field_of_spec spec in
      function
      | [] -> ()
      | vs ->
        let writer' = Writer.init () in
        List.iter ~f:(fun v -> Writer.add_field writer' (f v)) vs;
        Writer.concat_as_length_delimited writer ~src:writer' index
    end
  | Repeated (index, spec) ->
      let f = field_of_spec spec in
      fun vs -> List.iter ~f:(fun v -> Writer.write_field writer index (f v)) vs
  | Basic (index, spec) -> begin
      let f = field_of_spec spec in
      fun v -> match f v with
        | Varint 0 -> ()
        | Fixed_64_bit v when v = Int64.zero -> ()
        | Fixed_32_bit v when v = Int32.zero -> ()
        | Length_delimited {length = 0; _} -> ()
        | field -> Writer.write_field writer index field
    end
  | Oneof f -> fun v ->
    let Oneof_elem (index, spec, v) = f v in
    write writer (Basic (index, spec)) v

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Writer.t -> (a, Writer.t) compound_list -> a = fun writer -> function
  | Nil -> writer
  | Cons (compound, rest) ->
    fun v -> write writer compound v;
      serialize writer rest

let serialize spec = serialize (Writer.init ()) spec


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

  let repeated s = Repeated s
  let basic s = Basic s
  let oneof s = Oneof s
  let oneof_elem (a, b, c) = Oneof_elem (a, b, c)

  let ( ^:: ) a b = Cons (a, b)
  let nil = Nil
end
