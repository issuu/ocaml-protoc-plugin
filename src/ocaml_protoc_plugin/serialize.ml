open StdLabels
open Field

module S = Spec.Serialize
module C = S.C
open S

(* Take a list of fields and return a field *)
let serialize_message : (int * Field.t) list -> string =
 fun fields ->
  let writer = Writer.init () in
  List.iter ~f:(fun (index, field) -> Writer.write_field writer index field) fields;
  Writer.contents writer

let unsigned_varint v = Field.Varint v

let signed_varint v =
  let open! Infix.Int64 in
  let v =
    match v with
    | v when v < 0L -> v lsl 1 lxor (-1L)
    | v -> v lsl 1
  in
  Field.Varint v


let rec field_of_spec: type a. a spec -> a -> Field.t = function
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
      Field.length_delimited (Writer.contents writer)


let is_scalar: type a. a spec -> bool = function
  | String -> false
  | Bytes -> false
  | Message _ -> false
  | _ -> true

let rec write: type a. a compound -> Writer.t -> a -> unit = function
  | Basic (index, Message (to_proto), _) -> begin
      fun writer v ->
      let v = to_proto v in
      Writer.concat_as_length_delimited writer ~src:v index
    end
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
      | Proto2 default -> fun writer -> begin
          function
          | v when v = default -> ()
          | v -> Writer.write_field writer index (f v)
        end
      | Required -> fun writer v -> Writer.write_field writer index (f v)
    end
  | Basic_opt (index, spec) -> begin
    let f = field_of_spec spec in
    fun writer -> function
      | Some v -> Writer.write_field writer index (f v)
      | None -> ()
  end
  | Oneof f -> begin
      fun writer -> function
        | `not_set -> ()
        | v ->
            let Oneof_elem (index, spec, v) = f v in
            write (Basic (index, spec, Required)) writer v
    end

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. (a, Writer.t) compound_list -> Writer.t -> a = function
  | Nil -> fun writer -> writer
  | Cons (compound, rest) ->
    let cont = serialize rest in
    let write = write compound in
    fun writer v ->
      write writer v;
      cont writer

let in_extension_ranges extension_ranges index =
  List.exists ~f:(fun (start, end') -> index >= start && index <= end') extension_ranges


let serialize extension_ranges spec =
  let serialize = serialize spec in
  fun extensions ->
    let writer = Writer.init () in
    List.iter ~f:(function
        | (index, field) when in_extension_ranges extension_ranges index -> Writer.write_field writer index field
        | _ -> ()
      ) extensions;
    serialize writer


let%expect_test "signed varint" =
  let test v =
    let vl = Int64.of_int v in
    Printf.printf "signed_varint %LdL = %s\n" vl (signed_varint vl |> Field.show);
    ()
  in
  List.iter ~f:test [0; -1; 1; -2; 2; 2147483647; -2147483648];
  [%expect {|
    signed_varint 0L = (Field.Varint 0L)
    signed_varint -1L = (Field.Varint 1L)
    signed_varint 1L = (Field.Varint 2L)
    signed_varint -2L = (Field.Varint 3L)
    signed_varint 2L = (Field.Varint 4L)
    signed_varint 2147483647L = (Field.Varint 4294967294L)
    signed_varint -2147483648L = (Field.Varint 4294967295L) |}]
