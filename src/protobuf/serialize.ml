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

type _ compound =
  | Message: ('a -> Writer.t) -> 'a option compound
  | RepeatedMessage : ('a -> Writer.t) -> 'a list compound
  | Repeated : 'a spec -> 'a list compound
  | Oneof : ('a -> Writer.t) -> 'a compound
  | Basic : 'a spec -> 'a compound

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
 fun fields ->
  let writer = Writer.init () in
  List.iter ~f:(fun (index, field) -> Writer.write_field writer index field) fields;
  Writer.contents writer

type (_, _) compound_list =
  | Nil : ('a, 'a) compound_list
  | Cons :
      (int * 'a compound) * ('b, 'c) compound_list
      -> ('a -> 'b, 'c) compound_list

let ( ^:: ) a b = Cons (a, b)

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

let is_scalar: type a. a spec -> bool = function
  | String -> false
  | Bytes -> false
  | _ -> true

let rec write: type a. index:int -> Writer.t -> a compound -> a -> unit = fun ~index writer ->
  function
  | Message (to_proto) -> begin
      function
      | Some v ->
        let v = to_proto v in
        Writer.concat_as_length_delimited writer ~src:v index
      | None -> ()
    end
  | RepeatedMessage (to_proto) ->
    fun vs -> List.iter ~f:(fun v -> write ~index writer (Message to_proto) (Some v)) vs
  | Repeated spec when is_scalar spec -> begin
      let f = field_of_spec spec in
      function
      | [] -> ()
      | vs ->
        let writer' = Writer.init () in
        List.iter ~f:(fun v -> Writer.add_field writer' (f v)) vs;
        Writer.concat_as_length_delimited writer ~src:writer' index
    end
  | Repeated spec ->
      let f = field_of_spec spec in
      fun vs -> List.iter ~f:(fun v -> Writer.write_field writer index (f v)) vs
  | Basic spec -> begin
      let f = field_of_spec spec in
      fun v -> match f v with
        | Varint 0 -> ()
        | Fixed_64_bit v when v = Int64.zero -> ()
        | Fixed_32_bit v when v = Int32.zero -> ()
        | Length_delimited {length = 0; _} -> ()
        | field -> Writer.write_field writer index field
    end
  | Oneof f -> fun v ->
    Writer.concat ~src:(f v) writer

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Writer.t -> (a, Writer.t) compound_list -> a = fun writer -> function
  | Nil -> writer
  | Cons ((index, compound), rest) ->
    fun v -> write ~index writer compound v;
      serialize writer rest

let serialize spec = serialize (Writer.init ()) spec
