open Base
open Spec

type 'a default = Proto3 | Proto2 of 'a option | Required
type packed = Packed | Not_packed

type _ spec =
  | Double : float spec
  | Float : float spec

  | Int32 : Int32.t spec
  | UInt32 : Int32.t spec
  | SInt32 : Int32.t spec
  | Fixed32 : Int32.t spec
  | SFixed32 : Int32.t spec

  | Int32_int : int spec
  | UInt32_int : int spec
  | SInt32_int : int spec
  | Fixed32_int : int spec
  | SFixed32_int : int spec

  | UInt64 : Int64.t spec
  | Int64 : Int64.t spec
  | SInt64 : Int64.t spec
  | Fixed64 : Int64.t spec
  | SFixed64 : Int64.t spec

  | UInt64_int : int spec
  | Int64_int : int spec
  | SInt64_int : int spec
  | Fixed64_int : int spec
  | SFixed64_int : int spec

  | Bool : bool spec
  | String : string spec
  | Bytes : bytes spec
  | Enum: ('a -> int) -> 'a spec
  | Message: ('a -> Writer.t) -> 'a spec
  | Message_opt: ('a -> Writer.t) -> 'a option spec

type _ oneof_elem =
  | Oneof_elem : int * 'b spec * 'b -> unit oneof_elem

type _ compound =
  | Repeated : int * 'a spec * packed -> 'a list compound
  | Basic : int * 'a spec * 'a default -> 'a compound
  | Oneof : ('a -> unit oneof_elem) -> 'a compound

type (_, _) compound_list =
  | Nil : ('a, 'a) compound_list
  | Cons : 'a compound * ('b, 'c) compound_list -> ('a -> 'b, 'c) compound_list

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
 fun fields ->
  let writer = Writer.init () in
  List.iter ~f:(fun (index, field) -> Writer.write_field writer index field) fields;
  Writer.contents writer

let unsigned_varint v = Varint v

let signed_varint v =
  let open Int64 in
  let v =
    match v with
    | v when is_negative v -> v lsl 1 lxor (-1L)
    | v -> v lsl 1
  in
  Varint v


let rec field_of_spec: type a. a spec -> a -> Spec.field = function
  | Double -> fun v -> Fixed_64_bit (Int64.bits_of_float v)
  | Float -> fun v -> Fixed_32_bit (Int32.bits_of_float v)
  | Int64 -> unsigned_varint
  | Int64_int -> fun v -> unsigned_varint (Int64.of_int v)
  | UInt64 -> unsigned_varint
  | UInt64_int -> fun v -> unsigned_varint (Int64.of_int v)
  | SInt64 -> signed_varint
  | SInt64_int -> fun v -> signed_varint (Int64.of_int v)

  | Int32 -> fun v -> unsigned_varint (Int64.of_int32 v)
  | Int32_int -> fun v -> unsigned_varint (Int64.of_int v)
  | UInt32 -> fun v -> unsigned_varint (Int64.of_int32 v)
  | UInt32_int -> fun v -> unsigned_varint (Int64.of_int v)
  | SInt32 -> fun v -> signed_varint (Int64.of_int32 v)
  | SInt32_int -> fun v -> signed_varint (Int64.of_int v)

  | Fixed64 -> fixed_64_bit
  | Fixed64_int -> fun v -> Fixed_64_bit (Int64.of_int v)
  | SFixed64 -> fixed_64_bit
  | SFixed64_int -> fun v -> Fixed_64_bit (Int64.of_int v)
  | Fixed32 -> fixed_32_bit
  | Fixed32_int -> fun v -> Fixed_32_bit (Int32.of_int v)
  | SFixed32 -> fixed_32_bit
  | SFixed32_int -> fun v -> Fixed_32_bit (Int32.of_int v)

  | Bool -> fun v -> unsigned_varint (match v with | true -> 1L | false -> 0L)
  | String -> fun v -> Length_delimited {offset = 0; length = String.length v; data = v}
  | Bytes -> fun v -> Length_delimited {offset = 0; length = Bytes.length v; data = Bytes.to_string v}
  | Enum f ->
    let to_field = field_of_spec UInt64 in
    fun v -> f v |> Int64.of_int |> to_field
  | Message to_proto ->
    fun v ->
      let writer = to_proto v in
      Spec.length_delimited (Writer.contents writer)
  | Message_opt _ -> failwith "Cannot comply"


let is_scalar: type a. a spec -> bool = function
  | String -> false
  | Bytes -> false
  | Message _ -> false
  | Message_opt _ -> false
  | _ -> true

let rec write: type a. a compound -> Writer.t -> a -> unit = function
  | Basic (index, Message_opt (to_proto), _) -> begin
      fun writer -> function
      | Some v ->
        let v = to_proto v in
        Writer.concat_as_length_delimited writer ~src:v index
      | None -> ()
    end
  | Basic (index, Message (to_proto), _) ->
    fun writer v ->
      let v = to_proto v in
      Writer.concat_as_length_delimited writer ~src:v index
  | Repeated (index, Message to_proto, _) ->
    let write = write (Basic (index, Message to_proto, Required)) in
    fun writer vs -> List.iter ~f:(fun v -> write writer v) vs
  | Repeated (index, spec, Packed) when is_scalar spec -> begin
      let f = field_of_spec spec in
      fun writer -> function
      | [] -> ()
      | vs ->
        let writer' = Writer.init () in
        List.iter ~f:(fun v -> Writer.add_field writer' (f v)) vs;
        Writer.concat_as_length_delimited writer ~src:writer' index
    end
  | Repeated (index, spec, _) ->
      let f = field_of_spec spec in
      fun writer vs -> List.iter ~f:(fun v -> Writer.write_field writer index (f v)) vs
  | Basic (index, spec, default) -> begin
      let f = field_of_spec spec in
      match default with
      | Proto3 -> begin
          fun writer v -> match f v with
            | Varint 0L -> ()
            | Fixed_64_bit 0L -> ()
            | Fixed_32_bit 0l -> ()
            | Length_delimited {length = 0; _} -> ()
            | field -> Writer.write_field writer index field
        end
      | Proto2 _
      | Required -> fun writer v -> Writer.write_field writer index (f v)
    end
  | Oneof f ->
    fun writer v ->
      let Oneof_elem (index, spec, v) = f v in (* Ouch. We do not write oneof fields with default values??? *)
      write (Basic (index, spec, Required)) writer v

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. (a, Writer.t) compound_list -> Writer.t -> a = function
  | Nil -> fun writer -> writer
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write compound in
    fun writer v ->
      write writer v;
      cont writer

let serialize spec =
  let serialize = serialize spec in
  fun () -> serialize (Writer.init ())

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

  let int32_int = Int32_int
  let int64_int = Int64_int
  let uint32_int = UInt32_int
  let uint64_int = UInt64_int
  let sint32_int = SInt32_int
  let sint64_int = SInt64_int
  let fixed32_int = Fixed32_int
  let fixed64_int = Fixed64_int
  let sfixed32_int = SFixed32_int
  let sfixed64_int = SFixed64_int

  let bool = Bool
  let string = String
  let bytes = Bytes
  let enum f = Enum f
  let message f = Message f
  let message_opt f = Message_opt f

  let repeated (a, b, c) = Repeated (a, b, c)
  let basic (a, b, c) = Basic (a, b, c)
  let oneof s = Oneof s
  let oneof_elem (a, b, c) = Oneof_elem (a, b, c)

  let some v = Some v
  let none = None
  let proto2 v = Proto2 v
  let proto2_bytes v = Proto2 (Some (Bytes.of_string v))
  let proto3 = Proto3
  let required = Required

  let packed = Packed
  let not_packed = Not_packed

  let ( ^:: ) a b = Cons (a, b)
  let nil = Nil
end

module Test = struct
  let%test "signed_varint 0L"  = signed_varint 0L = Varint 0L
  let%test "signed_varint -1L" = signed_varint (-1L) = Varint 1L
  let%test "signed_varint 1L" = signed_varint 1L = Varint 2L
  let%test "signed_varint -2L" = signed_varint (-2L) = Varint 3L
  let%test "signed_varint 2147483647L"  = signed_varint 2147483647L = Varint 4294967294L
  let%test "signed_varint -2147483648L"  = signed_varint (-2147483648L) = Varint 4294967295L
end
