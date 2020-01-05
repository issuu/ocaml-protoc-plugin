(*) indicates breaking change

- [ ] Do not add unit argument for oneof constructors referencing empty messages

## 2.1.0: Not released yet
- [x] Add custom option to mangle names (modules, fields and enums) to
      more Ocaml idiomatic names (snake_cased)
- [x] Change type of deserialize error type to be an lower bound polymorphic variant
- [x] Rewrite type mapping to ensure that no name clashes can exist.
- [x] Fix bug in nested cursive types referencing wrong types
- [x] Add custom options, so options to ocaml\_protoc\_plugin can be
      embedded in .proto files
- [x] Support extensions
- [x] Allow use of message name Ocaml\_protoc\_plugin
- [x] `*`Do not treat oneof fields as required, adding a `not_set variant
      to all oneofs.
- [x] Avoid name clash with imported .proto files

## 2.0.0: 2019-10-20
- [x] Add examples
- [x] *Oneofs with only one element should not be a variant type
- [x] Add test when including proto files which defines the same package
- [x] Add google well know types (library `ocaml-protoc-plugin.google_types`).
- [x] *Move module to ocaml-protoc-plugin
- [x] Optimize deserialization of large nested structures
- [x] Provide pretty_printers aka deriving_show for `Result.error` and `Field.t`
- [x] Fix stack overflow when deserializing big nested structures
- [x] *Add option to not wrap single field type in records
- [x] Refactor type emitter to closely follow spec

## 1.0.0: 2019-10-12
- [x] Support enum aliasing
- [x] Avoid name clash with on 'name'
- [x] Fix code generation when argument contains a path
- [x] Refactor internal types to make serialization and
      deserialization type spec symmetrical.
- [x] Optimize deserialization for messages with max_id < 1024
- [x] Dont depend on Base in runtime
- [x] Slim runtime dependencies: Remove need for base, ocplib-endian
      and ppx_let
- [x] Honour [packed=...] flag.
- [x] Make fixed scalar types default to int32 and int64
- [x] Support proto2 specification
- [x] Add options to switch between int64|int32 and int
- [x] Fix name clash problem with special enum names
- [x] Refactor serializaton and deserialization to simplify emitted code
- [x] Eagerly evaluate serialization (for speed).

## 0.9: 2019-09-25
- [x] Initial Release
