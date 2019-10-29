open Spec.Descriptor.Google.Protobuf

type t = {
  type' : string;
  constructor: string;
  apply: string;
  deserialize_spec: string;
  serialize_spec: string;
}

val make:
  params:Parameters.t ->
  syntax:[< `Proto2 | `Proto3 ] ->
  is_cyclic: bool ->
  is_map_entry: bool ->
  has_extensions: bool ->
  scope:Scope.t ->
  fields:FieldDescriptorProto.t list -> OneofDescriptorProto.t list -> t
