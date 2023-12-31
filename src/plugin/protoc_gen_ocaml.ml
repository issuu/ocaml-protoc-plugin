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
  |> Ocaml_protoc_plugin.Reader.create
  |> Plugin.CodeGeneratorRequest.from_proto_exn

(* Write to stdout *)
let write response =
  Plugin.CodeGeneratorResponse.to_proto response
  |> Ocaml_protoc_plugin.Writer.contents
  |> output_string stdout


let parse_request Plugin.CodeGeneratorRequest.{file_to_generate = files_to_generate; parameter = parameters; proto_file = proto_files; compiler_version = _} =
  let params = Parameters.parse (Option.value ~default:"" parameters) in
  (* Find the correct file to process *)
  let target_proto_files = List.filter ~f:(fun Descriptor.FileDescriptorProto.{name; _} ->
      List.mem ~set:files_to_generate (Option.value_exn name)
    ) proto_files
  in
  let scope = Scope.init proto_files in
  let result =
    List.map ~f:(fun (proto_file : Descriptor.FileDescriptorProto.t) ->
        let scope = Scope.for_descriptor scope proto_file in
        Emit.parse_proto_file ~params scope proto_file
      ) target_proto_files
    |> List.map ~f:(fun (name, code) ->
        (Filename.basename name, code)
      )
  in
  (match params.debug with
   | true -> List.iter ~f:(fun (_, code) -> Printf.eprintf "%s\n%!" (Code.contents code)) result
   | false -> ());
  result

let () =
  let request = read () in
  let outputs = parse_request request in
  let response_of_output (name, code) =
    Plugin.CodeGeneratorResponse.File.make ~name ~content:(Code.contents code) ()
  in
  let response : Plugin.CodeGeneratorResponse.t =
    Plugin.CodeGeneratorResponse.make ~supported_features:1 ~file:(List.map ~f:response_of_output outputs) ()
  in
  write response
