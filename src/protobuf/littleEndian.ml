module type Integer = sig
  type t
  val zero: t
  val logand: t -> t -> t
  val of_int: int -> t
  val to_int: t -> int
  val shift_right_logical: t -> int -> t
  val shift_left: t -> int -> t
  val add: t -> t -> t
end

(** For Little-endian platform *)
module LittleEndian = struct
  let set_int (type t) (module T: Integer with type t = t) size buffer offset v =
    let open T in
    let rec inner acc = function
      | n when n = size -> ()
      | n ->
        let ch = logand acc (of_int 0xff) |> to_int |> Char.chr in
        Bytes.set buffer (n + offset) ch;
        inner (shift_right_logical acc 8) (n+1)
    in
    inner v 0

  let get_int (type t) (module T: Integer with type t = t) size buffer offset =
    let open T in
    let rec inner acc = function
      | 0 -> acc
      | n ->
        let v = String.get buffer (offset + n - 1) |> Char.code in
        let acc = add (shift_left acc 8) (of_int v) in
        inner acc (n - 1)
    in
    inner T.zero size

  let get_int64 = get_int (module Int64) 8
  let get_int32 = get_int (module Int32) 4
  let set_int64 = set_int (module Int64) 8
  let set_int32 = set_int (module Int32) 4

end

(** For Bigendian platform *)
module BigEndian = struct
  let set_int (type t) (module T: Integer with type t = t) size buffer offset v =
    let open T in
    let rec inner acc = function
      | n when n = size -> ()
      | n ->
        let ch = logand acc (of_int 0xff) |> to_int |> Char.chr in
        Bytes.set buffer ((size - n - 1) + offset) ch;
        inner (shift_right_logical acc 8) (n+1)
    in
    inner v 0

  let get_int (type t) (module T: Integer with type t = t) size buffer offset =
    let open T in
    let rec inner acc = function
      | 0 -> acc
      | n ->
        let v = String.get buffer (offset + size - n) |> Char.code in
        let acc = add (shift_left acc 8) (of_int v) in
        inner acc (n - 1)
    in
    inner T.zero size

  let get_int64 = get_int (module Int64) 8
  let get_int32 = get_int (module Int32) 4
  let set_int64 = set_int (module Int64) 8
  let set_int32 = set_int (module Int32) 4

end

let get_int64 = match Sys.big_endian with true -> BigEndian.get_int64 | false -> LittleEndian.get_int64
let get_int32 = match Sys.big_endian with true -> BigEndian.get_int32 | false -> LittleEndian.get_int32
let set_int64 = match Sys.big_endian with true -> BigEndian.set_int64 | false -> LittleEndian.set_int64
let set_int32 = match Sys.big_endian with true -> BigEndian.set_int32 | false -> LittleEndian.set_int32


module Test = struct
  let test _ =
    let (_:bool) =
      let buffer = Bytes.create 8 in
      let v = 1234567876543L in
      set_int64 buffer 0 v;
      get_int64 (Bytes.to_string buffer) 0 = v || failwith "int64"
    in
    let (_:bool) =
      let buffer = Bytes.create 4 in
      let v = 12345673l in
      set_int32 buffer 0 v;
      get_int32 (Bytes.to_string buffer) 0 = v || failwith "int32"
    in
    ()
end
