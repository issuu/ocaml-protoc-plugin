open Base

let read_all in_channel =
  let rec inner buffer =
    let b = Bytes.create 1024 in
    match Caml.input in_channel b 0 1024 with
    | 1024 ->
      Buffer.add_bytes buffer b;
      inner buffer
    | read ->
      Buffer.add_subbytes buffer b ~pos:0 ~len:read;
      Buffer.contents_bytes buffer
  in
  inner (Buffer.create 1024)

(* Read from stdin *)
let read () =
  read_all Caml.stdin
  |> Pbrt.Decoder.of_bytes
  |> Spec.Plugin.Pb.decode_code_generator_request

(* Write to stdout *)
let write response =
  let encoder = Pbrt.Encoder.create () in
  Spec.Plugin.Pb.encode_code_generator_response response encoder;
  let response = Pbrt.Encoder.to_bytes encoder in
  Caml.(output_bytes stdout) response

let () =
  let request = read () in
  let outputs = Emit.parse_request request in
  let response_of_output (name, code) =
    let insertion_point = None in
    let content = Some (Code.contents code) in
    Spec.Plugin.{name; insertion_point; content}
  in
  let response : Spec.Plugin.code_generator_response =
    {error = None; file = List.map ~f:response_of_output outputs}
  in
  write response
