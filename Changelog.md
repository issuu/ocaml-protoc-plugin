- [ ] Unify spec
- [ ] Support enum entries with same id

## 0.9.1: Unreleased
- [x] Dont depend on Base in runtime
- [x] Slim runtime dependencies: Remove need for base, ocplib-endian
      and ppx_let
- [x] Honour [packed=...] flag.
- [x] Make fixed scalar types default to int32 and int64
- [x] Always serialize proto2 type fields
- [x] Support default values (proto2)
- [x] Support required (proto2) fields
- [x] Add options to switch between int64|int32 and int
- [x] Fix name clash problem with special enum names
- [x] Refactor serializaton and deserialization to simplify emitted code
- [x] Eagerly evaluate serialization (for speed).

## 0.9: 2019-09-25
- [x] Initial Release
