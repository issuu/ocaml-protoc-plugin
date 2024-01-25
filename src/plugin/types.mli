open Spec.Descriptor.Google.Protobuf

type t = {
  type' : string;
  constructor: string;
  apply: string;
  deserialize_spec: string;
  serialize_spec: string;
  default_constructor_sig: string;
  default_constructor_impl: string;
}

type field_spec = {
  typestr : string;
  serialize_spec: string;
  deserialize_spec: string;
}

val spec_of_field:
  params:Parameters.t ->
  syntax:[ `Proto2 | `Proto3 ] ->
  scope:Scope.t -> FieldDescriptorProto.t -> field_spec

val make:
  params:Parameters.t ->
  syntax:[ `Proto2 | `Proto3 ] ->
  is_cyclic: bool ->
  is_map_entry: bool ->
  extension_ranges: (int*int) list ->
  scope:Scope.t ->
  fields:FieldDescriptorProto.t list -> OneofDescriptorProto.t list -> t
