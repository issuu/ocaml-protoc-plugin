open StdLabels
module Descriptor = Spec.Descriptor.Google.Protobuf
module Plugin = Spec.Plugin.Google.Protobuf.Compiler

let read_all in_channel =
  let rec inner buffer =
    let b = Bytes.create 1024 in
    match input in_channel b 0 1024 with
    | 1024 ->
      Buffer.add_bytes buffer b;
      inner buffer
    | read ->
      Buffer.add_subbytes buffer b 0 read;
      Buffer.contents buffer
  in
  inner (Buffer.create 1024)

(* Read from stdin *)
let read () =
  read_all stdin
  |> Protobuf.Reader.create
  |> Plugin.CodeGeneratorRequest.from_proto
  |> function
  | Ok v -> v
  | Error _ -> failwith "Could not decode generator request"

(* Write to stdout *)
let write response =
  Plugin.CodeGeneratorResponse.to_proto response
  |> Protobuf.Writer.contents
  |> output_string stdout

let () =
  let request = read () in
  let outputs = Emit.parse_request request in
  let response_of_output (name, code) =
    let insertion_point = None in
    let content = Some (Code.contents code) in
    Plugin.CodeGeneratorResponse.File.{name; insertion_point; content}
  in
  let response : Plugin.CodeGeneratorResponse.t =
    {error = None; file = List.map ~f:response_of_output outputs}
  in
  write response
