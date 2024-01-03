open StdLabels
open Field

module S = Spec.Serialize
module C = S.C
open S

let varint ~signed v =
  let open Infix.Int64 in
  let v = match signed with
    | true when v < 0L -> v lsl 1 lxor (-1L)
    | true -> v lsl 1
    | false -> v
  in
  Field.Varint v

let varint_unboxed ~signed v =
  let v = match signed with
    | true when v < 0 -> v lsl 1 lxor (-1)
    | true -> v lsl 1
    | false -> v
  in
  Field.Varint_unboxed v


let rec field_of_spec: type a. a spec -> a -> Field.t = function
  | Double -> fun v -> Fixed_64_bit (Int64.bits_of_float v)
  | Float -> fun v -> Fixed_32_bit (Int32.bits_of_float v)
  | Int64 -> varint ~signed:false
  | Int64_int -> varint_unboxed ~signed:false
  | UInt64 -> varint ~signed:false
  | UInt64_int -> varint_unboxed ~signed:false
  | SInt64 -> varint ~signed:true
  | SInt64_int -> varint_unboxed ~signed:true
  | Int32 -> fun v -> varint ~signed:false (Int64.of_int32 v)
  | Int32_int -> varint_unboxed ~signed:false
  | UInt32 -> fun v -> varint ~signed:false (Int64.of_int32 v)
  | UInt32_int -> varint_unboxed ~signed:false
  | SInt32 -> fun v -> varint ~signed:true (Int64.of_int32 v)
  | SInt32_int -> varint_unboxed ~signed:true

  | Fixed64 -> fixed_64_bit
  | Fixed64_int -> fun v -> Fixed_64_bit (Int64.of_int v)
  | SFixed64 -> fixed_64_bit
  | SFixed64_int -> fun v -> Fixed_64_bit (Int64.of_int v)
  | Fixed32 -> fixed_32_bit
  | Fixed32_int -> fun v -> Fixed_32_bit (Int32.of_int v)
  | SFixed32 -> fixed_32_bit
  | SFixed32_int -> fun v -> Fixed_32_bit (Int32.of_int v)

  | Bool -> fun v -> varint_unboxed ~signed:false (match v with | true -> 1 | false -> 0)
  | String -> fun v -> Length_delimited {offset = 0; length = String.length v; data = v}
  | Bytes -> fun v -> Length_delimited {offset = 0; length = Bytes.length v; data = Bytes.unsafe_to_string v}
  | Enum f ->
    let to_field = field_of_spec UInt64_int in
    fun v -> f v |> to_field
  | Message to_proto -> (* Consider inlining this into write *)
    fun v ->
      let writer = Writer.init () in
      let writer = to_proto writer v in
      length_delimited (Writer.contents writer)

let is_scalar: type a. a spec -> bool = function
  | String -> false
  | Bytes -> false
  | Message _ -> false
  | _ -> true

let rec write: type a. a compound -> Writer.t -> a -> unit = function
  | Basic (index, Message to_proto, _) -> begin
      fun writer v ->
      let done_f = Writer.add_length_delimited_field_header writer index in
      let _writer = to_proto writer v in
      done_f ()
    end
  | Repeated (index, Message to_proto, _) ->
    let write = write (Basic (index, Message to_proto, None)) in
    fun writer vs -> List.iter ~f:(fun v -> write writer v) vs
  | Repeated (index, spec, Packed) when is_scalar spec -> begin
      let f = field_of_spec spec in
      fun writer -> function
      | [] -> ()
      | vs ->
         let done_f = Writer.add_length_delimited_field_header writer index in
         List.iter ~f:(fun v -> Writer.add_field writer (f v)) vs;
         done_f ()
    end
  | Repeated (index, spec, _) ->
      let f = field_of_spec spec in
      fun writer vs -> List.iter ~f:(fun v -> Writer.write_field writer index (f v)) vs
  | Basic (index, spec, default) -> begin
      let f = field_of_spec spec in
      match default with
      | Some default -> fun writer -> begin
          function
          | v when v = default -> ()
          | v -> Writer.write_field writer index (f v)
        end
      | None -> fun writer v -> Writer.write_field writer index (f v)
    end
  | Basic_opt (index, Message to_proto) -> begin
     fun writer -> function
       | Some v ->
          let done_f = Writer.add_length_delimited_field_header writer index in
          let _ = to_proto writer v in
          done_f ()
       | None -> ()
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
            write (Basic (index, spec, None)) writer v
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
  fun extensions writer ->
    List.iter ~f:(function
        | (index, field) when in_extension_ranges extension_ranges index -> Writer.write_field writer index field
        | _ -> ()
      ) extensions;
    serialize writer

let%expect_test "signed varint" =

  let test v =
    let vl = Int64.of_int v in
    Printf.printf "varint ~signed:true %LdL = %s\n" vl (varint ~signed:true vl |> Field.show);
    Printf.printf "varint_unboxed ~signed:true %d = %s\n" v (varint_unboxed ~signed:true v |> Field.show);

    let vl' = (varint ~signed:true vl |> Deserialize.read_varint ~signed:true ~type_name:"") in
    Printf.printf "Signed: %LdL = %LdL (%b)\n" vl vl' (vl = vl');

    let vl' = (varint ~signed:false vl |> Deserialize.read_varint ~signed:false ~type_name:"") in
    Printf.printf "Unsigned: %LdL = %LdL (%b)\n" vl vl' (vl = vl');

    let v' = (varint_unboxed ~signed:true v |> Deserialize.read_varint_unboxed ~signed:true ~type_name:"") in
    Printf.printf "Signed unboxed: %d = %d (%b)\n" v v' (v = v');

    let v' = (varint_unboxed ~signed:false v |> Deserialize.read_varint_unboxed ~signed:false ~type_name:"") in
    Printf.printf "Unsigned unboxed: %d = %d (%b)\n" v v' (v=v');
    ()
  in
  List.iter ~f:test [0; -1; 1; -2; 2; 2147483647; -2147483648; Int.max_int; Int.min_int; ];
  [%expect {|
    varint ~signed:true 0L = (Field.Varint 0L)
    varint_unboxed ~signed:true 0 = (Field.Varint_unboxed 0)
    Signed: 0L = 0L (true)
    Unsigned: 0L = 0L (true)
    Signed unboxed: 0 = 0 (true)
    Unsigned unboxed: 0 = 0 (true)
    varint ~signed:true -1L = (Field.Varint 1L)
    varint_unboxed ~signed:true -1 = (Field.Varint_unboxed 1)
    Signed: -1L = -1L (true)
    Unsigned: -1L = -1L (true)
    Signed unboxed: -1 = -1 (true)
    Unsigned unboxed: -1 = -1 (true)
    varint ~signed:true 1L = (Field.Varint 2L)
    varint_unboxed ~signed:true 1 = (Field.Varint_unboxed 2)
    Signed: 1L = 1L (true)
    Unsigned: 1L = 1L (true)
    Signed unboxed: 1 = 1 (true)
    Unsigned unboxed: 1 = 1 (true)
    varint ~signed:true -2L = (Field.Varint 3L)
    varint_unboxed ~signed:true -2 = (Field.Varint_unboxed 3)
    Signed: -2L = -2L (true)
    Unsigned: -2L = -2L (true)
    Signed unboxed: -2 = -2 (true)
    Unsigned unboxed: -2 = -2 (true)
    varint ~signed:true 2L = (Field.Varint 4L)
    varint_unboxed ~signed:true 2 = (Field.Varint_unboxed 4)
    Signed: 2L = 2L (true)
    Unsigned: 2L = 2L (true)
    Signed unboxed: 2 = 2 (true)
    Unsigned unboxed: 2 = 2 (true)
    varint ~signed:true 2147483647L = (Field.Varint 4294967294L)
    varint_unboxed ~signed:true 2147483647 = (Field.Varint_unboxed 4294967294)
    Signed: 2147483647L = 2147483647L (true)
    Unsigned: 2147483647L = 2147483647L (true)
    Signed unboxed: 2147483647 = 2147483647 (true)
    Unsigned unboxed: 2147483647 = 2147483647 (true)
    varint ~signed:true -2147483648L = (Field.Varint 4294967295L)
    varint_unboxed ~signed:true -2147483648 = (Field.Varint_unboxed 4294967295)
    Signed: -2147483648L = -2147483648L (true)
    Unsigned: -2147483648L = -2147483648L (true)
    Signed unboxed: -2147483648 = -2147483648 (true)
    Unsigned unboxed: -2147483648 = -2147483648 (true)
    varint ~signed:true 4611686018427387903L = (Field.Varint 9223372036854775806L)
    varint_unboxed ~signed:true 4611686018427387903 = (Field.Varint_unboxed -2)
    Signed: 4611686018427387903L = 4611686018427387903L (true)
    Unsigned: 4611686018427387903L = 4611686018427387903L (true)
    Signed unboxed: 4611686018427387903 = -1 (false)
    Unsigned unboxed: 4611686018427387903 = 4611686018427387903 (true)
    varint ~signed:true -4611686018427387904L = (Field.Varint 9223372036854775807L)
    varint_unboxed ~signed:true -4611686018427387904 = (Field.Varint_unboxed -1)
    Signed: -4611686018427387904L = -4611686018427387904L (true)
    Unsigned: -4611686018427387904L = -4611686018427387904L (true)
    Signed unboxed: -4611686018427387904 = -1 (false)
    Unsigned unboxed: -4611686018427387904 = -4611686018427387904 (true) |}]
