- [ ] Add option to mangle names to snake_case
- [ ] Add examples
- [ ] Test includes of files defining the same package
- [ ] Do not add unit argument for oneof constructors referencing empty messages

## 2.0.0: Unreleased
- [ ] Add google well know types
- [ ] Move module to ocaml-protoc-plugin
- [ ] Split runtime to a seperate module
- [ ] Hide module aliases not needed by users
- [ ] Optimize happy path when deserializing
- [ ] Optimize deserialization of large nested structures
- [x] Fix stack overflow when deserializing big nested structures
- [x] *Add option to not wrap single field type in records
- [x] Rewrite type emitter to closely follow spec

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

(*) indicates breaking change
