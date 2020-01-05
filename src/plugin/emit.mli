open Spec.Descriptor.Google.Protobuf
val parse_proto_file:
  params:Parameters.t ->
  Scope.t -> FileDescriptorProto.t -> string * Code.t
