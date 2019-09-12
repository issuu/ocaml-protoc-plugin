open Core_kernel
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
  | Message : ('a -> string) -> 'a option spec
  | Enum : ('a -> int) -> 'a spec
  | Repeated : 'a spec -> 'a list spec
  | Oneof : ('a -> int * field) -> 'a spec

(* Take a list of fields and return a field *)
let serialize_message : (int * field) list -> string =
  fun fields ->
  let buffer = Protobuffer.init () in
  List.iter ~f:(fun (index, field) -> Protobuffer.write_field buffer index field) fields;
  Protobuffer.contents buffer

type (_, _) protobuf_type_list =
  | Nil : ('a, 'a) protobuf_type_list
  | Cons :
      (int * 'a spec) * ('b, 'c) protobuf_type_list
      -> ('a -> 'b, 'c) protobuf_type_list

let ( ^:: ) a b = Cons (a, b)

let unsigned_varint v = Varint v

let signed_varint v =
  let v =
    match v with
    | v when v < 0 -> (((v * -1) - 1) * 2) + 1
    | v -> v * 2
  in
  Varint v

let%test _ = signed_varint 0 = Varint 0

let%test _ = signed_varint (-1) = Varint 1

let%test _ = signed_varint 1 = Varint 2

let%test _ = signed_varint (-2) = Varint 3

let%test _ = signed_varint 2147483647 = Varint 4294967294

let%test _ = signed_varint (-2147483648) = Varint 4294967295

(** Allow emitted code to present a protobuf specification. *)
let rec serialize : type a. Protobuffer.t -> (a, Protobuffer.t) protobuf_type_list -> a =
 fun buffer -> function
  | Nil -> buffer
  | Cons ((index, Double), rest) ->
    fun v ->
      (match v with
      | 0.0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_64_bit (Int64.bits_of_float v)));
      serialize buffer rest
  | Cons ((index, Float), rest) ->
    fun v ->
      (match v with
      | 0.0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_32_bit (Int32.bits_of_float v)));
      serialize buffer rest
  | Cons ((index, Int64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, UInt64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Int32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v ->
        let v = v land 0xffffffff in
        Protobuffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Fixed32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, Fixed64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SFixed32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SFixed64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)));
      serialize buffer rest
  | Cons ((index, SInt32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (signed_varint v));
      serialize buffer rest
  | Cons ((index, SInt64), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (signed_varint v));
      serialize buffer rest
  | Cons ((index, UInt32), rest) ->
    fun v ->
      (match v with
      | 0 -> ()
      | v -> Protobuffer.write_field buffer index (unsigned_varint v));
      serialize buffer rest
  | Cons ((index, Bool), rest) ->
    fun v ->
      (match v with
      | false -> ()
      | true -> Protobuffer.write_field buffer index (unsigned_varint 1));
      serialize buffer rest
  | Cons ((index, String), rest) ->
    fun v ->
      (match v with
      | "" -> ()
      | s -> Protobuffer.write_field buffer index (Length_delimited s));
      serialize buffer rest
  | Cons ((index, Bytes), rest) ->
    fun v ->
      Protobuffer.write_field buffer index (Length_delimited (Bytes.to_string v));
      serialize buffer rest
  | Cons ((index, Message to_string), rest) ->
    fun v ->
      (match v with
      | None -> ()
      | Some msg -> Protobuffer.write_field buffer index (Length_delimited (to_string msg)));
      serialize buffer rest
  | Cons ((index, Enum to_int), rest) ->
    fun v ->
      (match to_int v with
      | 0 -> ()
      | n -> Protobuffer.write_field buffer index (unsigned_varint n));
      serialize buffer rest
  | Cons ((_index, Oneof f), rest) ->
    (* Oneof fields ignores the initial index *)
    fun v ->
     let index, v = f v in
     Protobuffer.write_field buffer index v;
     serialize buffer rest
  (* Repeated fields - Not packed *)
  | Cons ((index, Repeated (Message to_string)), rest) ->
    fun v ->
      List.iter v ~f:(function
          | None -> failwith "Repeated message cannot be null"
          | Some msg ->
            Protobuffer.write_field buffer index (Length_delimited (to_string msg)));
      serialize buffer rest
  | Cons ((index, Repeated String), rest) ->
    fun v ->
      List.iter v ~f:(fun msg -> Protobuffer.write_field buffer index (Length_delimited msg));
      serialize buffer rest
  | Cons ((index, Repeated Bytes), rest) ->
    fun v ->
      List.iter v ~f:(fun msg ->
          Protobuffer.write_field buffer index (Length_delimited (Bytes.to_string msg)));
      serialize buffer rest
  (* Repeated packed fields *)
  | Cons ((index, Repeated Double), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v ->
          Protobuffer.write_field value_buffer index (Fixed_64_bit (Int64.bits_of_float v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Float), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v ->
          Protobuffer.write_field value_buffer index (Fixed_32_bit (Int32.bits_of_float v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Int64), rest) ->
    (* TODO: This should not be encoded as a signed varint, but we should handle negative numbers here *)
    fun vs ->
     let value_buffer = Protobuffer.init () in
     List.iter ~f:(fun v -> Protobuffer.write_field buffer index (signed_varint v)) vs;
     Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
     serialize buffer rest
  | Cons ((index, Repeated UInt64), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter ~f:(fun v -> Protobuffer.write_field buffer index (unsigned_varint v)) vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Int32), rest) ->
    (* TODO: This should not be encoded as a signed varint, but we should handle negative numbers here *)
    fun vs ->
     let value_buffer = Protobuffer.init () in
     List.iter ~f:(fun v -> Protobuffer.write_field buffer index (unsigned_varint v)) vs;
     Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
     serialize buffer rest
  | Cons ((index, Repeated Fixed32), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Fixed64), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SFixed32), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (Fixed_32_bit (Int32.of_int_exn v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SFixed64), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (Fixed_64_bit (Int64.of_int_exn v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SInt32), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter ~f:(fun v -> Protobuffer.write_field buffer index (signed_varint v)) vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated SInt64), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter ~f:(fun v -> Protobuffer.write_field buffer index (signed_varint v)) vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated UInt32), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter ~f:(fun v -> Protobuffer.write_field buffer index (unsigned_varint v)) vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated Bool), rest) ->
    let int_of_bool = function
      | false -> 0
      | true -> 1
    in
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (unsigned_varint (int_of_bool v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((index, Repeated (Enum to_int)), rest) ->
    fun vs ->
      let value_buffer = Protobuffer.init () in
      List.iter
        ~f:(fun v -> Protobuffer.write_field buffer index (unsigned_varint (to_int v)))
        vs;
      Protobuffer.write_field buffer index (Length_delimited (Protobuffer.contents value_buffer));
      serialize buffer rest
  | Cons ((_, Repeated (Repeated _)), _) ->
    failwith "Chained repeated fields not supported"
  | Cons ((_, Repeated (Oneof _)), _) -> failwith "Oneof fields cannot be repeated"

let serialize spec = serialize (Protobuffer.init ()) spec
