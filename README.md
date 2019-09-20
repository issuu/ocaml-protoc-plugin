# Ocaml protoc plugin

[![BuildStatus](https://travis-ci.org/issuu/ocaml-protoc-plugin?branch=master)](https://travis-ci.org/issuu/ocaml-protoc-plugin)
The goal of Ocaml protoc plugin is to create an up to date plugin for
the google protobuf compiler (protoc) to generate ocaml types and
serialization and de-serialization.

The main features include:
* Messages are mapped to idiomatic OCaml types
* Support service descriptions
* proto3 compatibility

## Comparisson with other OCaml protobuf handlers.

| Feature           | ocaml-protoc     | ocaml-pb-plugin | ocaml-protoc-plugin |
| -------           | ------------     | --------------- | ------------------- |
| Ocaml types       | Supported        | Defined runtime | Supported           |
| Service endpoints | Not supported    | N/A             | Supported           |
| proto3            | Partly supported | supported       | Supported           |

(ocaml-protoc release 1.2.0 does not yet fully support proto3, the
master branch does, however)

## Types
Basic types are mapped trivially to ocaml types:

Primitive types:
| Protobuf Type | Ocaml type      |
| ------------- | ----------      |
| Integers      | int             |
| Real          | float           |
| string        | string          |
| bytes         | bytes           |

A message <name> declaration is compiled to a module <Name> with a record type
`t`. However, messages without any fields are mapped to unit.

Packages are trivially mapped to modules.
Included proto files (<name>.proto) are assumed to have been compiled
to <name>.ml, and types in included proto files are referenced by
their fill name.

Compound types are mapped like:
| Protobuf Type | Ocaml type                                                            |
| ------------- | ----------                                                              |
| oneof         | Polymorphic variants: `[ \`Field1 of fieldtype1 \| \`Field1 of fieldtype2 ]` |
| repeated 'a   | 'a list                                                                 |
| message       | message option                                                         |
| enum          | Abstrace data types: `Enum1 \| Enum2 \| Enum3`                           |
| map<'a, 'b>   | ('a * 'b) list |

## Invocation
If the plugin is available in the path as `protoc-gen-ocaml`, then you
can generate the ocaml code by running

```
  protoc --ocaml_out=. --ocaml_opt=<options> file.proto
```

`Options` control the code generated.

| Option      | Description                         | Example                   |
| ----------- | ------------------------------      | -----------------------   |
| annot       | Type annotations.                   | `annot=[@@deriving show]` |
| debug       | Enable debugging                    | `debug`                   |
| open        | Add open at top of generated files. May be given multiple times | `open=Base.Sexp           |


Parameters are seperated by `;`

If `protoc-gen-ocaml` is not located in the path, it is possible to
specify the exact path to the plugin:

```
protoc --plugin=protoc-gen-ocaml=../plugin/ocaml-protocol-plugin.exe --ocaml_out=. <file>.proto
```

### Older versions of protoc
It seems that the `--ocaml_opt` flag may not be supported by older
versions of the proto compiler. As an alternative, options can also be
passed with the `--ocaml_out` flag:

```
protoc --plugin=protoc-gen-ocaml=../plugin/ocaml.exe --ocaml_out=annot=debug;[@@deriving show { with_path = false }, eq]:. <file>.proto
```

### Using dune
Below is a dune rule for generating code for `test.proto`:
```
(rule
 (targets test.ml)
 (deps
  (:proto test.proto))
 (action
  (run protoc -I .  "--ocaml_opt=annot=[@@deriving show { with_path = false }, eq]" --ocaml_out=. %{proto})))
```

## Service interface
!Note this is deprecated, and will change
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
