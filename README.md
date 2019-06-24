# Ocaml protoc plugin

The goal of Ocaml protoc plugin is to create an up to date plugin for
the google protobug compiler (protoc) to generate ocaml types and
serialization and deserialization.

The main features should include:
* Messages are mapped to idomatic ocaml types
* Support service descriptions
* proto3 compatibility

## Differences from existing tools:

| Feature           | ocaml-protoc  | ocaml-pb-plugin | ocaml-protoc-plugin |
| -------           | ------------  | --------------- | ------------------- |
| Ocaml types       | Supported     | Defined runtime | Supported           |
| Service endpoints | Not supported | Unknown         | Supported           |
| proto3            | Not supported | Supported       | Supported           |


## Design
This tool will create a binary which will read a
`CodeGeneratorRequest` protobuf message, and produce a
`CodeGeneratorResponse`.

Serialization and deserialization would be based on existing code,
either from ocaml-protoc or ocaml-pb, whichever is the simpletst.

### Service definitions
Service definitions will add function on the form:

```ocaml
module IO = struct
  'a deferred;
end

val call: (string->string IO.deferred) -> request_type ->
  (response_type, [> `Some_error_type)] result IO.deferred

val service: (request_type -> (response_type, [> ]) result
IO.deferred]) -> string -> string, [> ] result IO.Deferred

```

The call function will take a `string -> string` function, which
implement message sending -> receiving.

The service function is a `string -> string` function which takes a
handler working over the actual message types.

Of course the the functions work of an IO monad.
