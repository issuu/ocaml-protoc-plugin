open Core_kernel
module T = Spec.Plugin

let tap ~f x = f x; x

(** Read from stdin *)
let read () =
  In_channel.stdin
  |> In_channel.input_all
  |> Bytes.of_string
  |> Pbrt.Decoder.of_bytes
  |> Spec.Plugin.Pb.decode_code_generator_request

(* Write to stdout *)
let write response =
  Pbrt.Encoder.create ()
  |> tap ~f:(Spec.Plugin.Pb.encode_code_generator_response response)
  |> Pbrt.Encoder.to_bytes
  |> Bytes.to_string
  |> Out_channel.output_string Out_channel.stdout

let () =
  let request = read () in
  Emit.parse_request request;
  let response = Spec.Plugin.default_code_generator_response () in
  write response
