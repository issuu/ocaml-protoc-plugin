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
