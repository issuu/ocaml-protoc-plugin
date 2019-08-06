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
  let outputs = Emit.parse_request request in
  let response_of_output (name, code) =
    let insertion_point = None in
    let content = Some (Code.dumps code) in
    Spec.Plugin.{name; insertion_point; content}
  in
  let response : Spec.Plugin.code_generator_response =
    {error = None; file = List.map ~f:response_of_output outputs}
  in
  write response
