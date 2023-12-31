(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
  Source: google/protobuf/compiler/plugin.proto
  Syntax: proto2
  Parameters:
    debug=false
    annot=''
    opens=[]
    int64_as_int=true
    int32_as_int=true
    fixed_as_int=false
    singleton_record=false
*)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]
(**/**)
module Imported'modules = struct
  module Descriptor = Descriptor
end
(**/**)
module Google = struct
  module Protobuf = struct
    module Compiler = struct
      module rec Version : sig
        val name': unit -> string
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        val make : ?major:int -> ?minor:int -> ?patch:int -> ?suffix:string -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        let name' () = "plugin.google.protobuf.compiler.Version"
        type t = { major: int option; minor: int option; patch: int option; suffix: string option }
        let make =
          fun ?major ?minor ?patch ?suffix () ->

          { major; minor; patch; suffix }

        let to_proto =
          let apply = fun ~f:f' { major; minor; patch; suffix } -> f' [] major minor patch suffix in
          let spec = Runtime'.Serialize.C.( basic_opt (1, int32_int) ^:: basic_opt (2, int32_int) ^:: basic_opt (3, int32_int) ^:: basic_opt (4, string) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t

        let from_proto_exn =
          let constructor = fun _extensions major minor patch suffix -> { major; minor; patch; suffix } in
          let spec = Runtime'.Deserialize.C.( basic_opt (1, int32_int) ^:: basic_opt (2, int32_int) ^:: basic_opt (3, int32_int) ^:: basic_opt (4, string) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer
          let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)

      end
      and CodeGeneratorRequest : sig
        val name': unit -> string
        type t = { file_to_generate: string list; parameter: string option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list; compiler_version: Version.t option }
        val make : ?file_to_generate:string list -> ?parameter:string -> ?proto_file:Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> ?compiler_version:Version.t -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorRequest"
        type t = { file_to_generate: string list; parameter: string option; proto_file: Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.t list; compiler_version: Version.t option }
        let make =
          fun ?file_to_generate ?parameter ?proto_file ?compiler_version () ->
          let file_to_generate = match file_to_generate with Some v -> v | None -> [] in
          let proto_file = match proto_file with Some v -> v | None -> [] in
          { file_to_generate; parameter; proto_file; compiler_version }

        let to_proto =
          let apply = fun ~f:f' { file_to_generate; parameter; proto_file; compiler_version } -> f' [] file_to_generate parameter proto_file compiler_version in
          let spec = Runtime'.Serialize.C.( repeated (1, string, not_packed) ^:: basic_opt (2, string) ^:: repeated (15, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.to_proto t)), not_packed) ^:: basic_opt (3, (message (fun t -> Version.to_proto t))) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t

        let from_proto_exn =
          let constructor = fun _extensions file_to_generate parameter proto_file compiler_version -> { file_to_generate; parameter; proto_file; compiler_version } in
          let spec = Runtime'.Deserialize.C.( repeated (1, string, not_packed) ^:: basic_opt (2, string) ^:: repeated (15, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.FileDescriptorProto.from_proto_exn t)), not_packed) ^:: basic_opt (3, (message (fun t -> Version.from_proto_exn t))) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer
          let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)

      end
      and CodeGeneratorResponse : sig
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make : ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end
        val name': unit -> string
        type t = { error: string option; supported_features: int option; file: CodeGeneratorResponse.File.t list }
        val make : ?error:string -> ?supported_features:int -> ?file:CodeGeneratorResponse.File.t list -> unit -> t
        val to_proto: t -> Runtime'.Writer.t
        val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
        val from_proto_exn: Runtime'.Reader.t -> t
      end = struct
        module rec Feature : sig
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          val to_int: t -> int
          val from_int: int -> t Runtime'.Result.t
          val from_int_exn: int -> t
        end = struct
          type t = FEATURE_NONE | FEATURE_PROTO3_OPTIONAL
          let to_int = function
            | FEATURE_NONE -> 0
            | FEATURE_PROTO3_OPTIONAL -> 1

          let from_int_exn = function
            | 0 -> FEATURE_NONE
            | 1 -> FEATURE_PROTO3_OPTIONAL
            | n -> Runtime'.Result.raise (`Unknown_enum_value n)

          let from_int e = Runtime'.Result.catch (fun () -> from_int_exn e)
        end
        and File : sig
          val name': unit -> string
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          val make : ?name:string -> ?insertion_point:string -> ?content:string -> ?generated_code_info:Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t -> unit -> t
          val to_proto: t -> Runtime'.Writer.t
          val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
          val from_proto_exn: Runtime'.Reader.t -> t
        end = struct
          let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse.File"
          type t = { name: string option; insertion_point: string option; content: string option; generated_code_info: Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.t option }
          let make =
            fun ?name ?insertion_point ?content ?generated_code_info () ->

            { name; insertion_point; content; generated_code_info }

          let to_proto =
            let apply = fun ~f:f' { name; insertion_point; content; generated_code_info } -> f' [] name insertion_point content generated_code_info in
            let spec = Runtime'.Serialize.C.( basic_opt (1, string) ^:: basic_opt (2, string) ^:: basic_opt (15, string) ^:: basic_opt (16, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.to_proto t))) ^:: nil ) in
            let serialize = Runtime'.Serialize.serialize [] (spec) in
            fun t -> apply ~f:serialize t

          let from_proto_exn =
            let constructor = fun _extensions name insertion_point content generated_code_info -> { name; insertion_point; content; generated_code_info } in
            let spec = Runtime'.Deserialize.C.( basic_opt (1, string) ^:: basic_opt (2, string) ^:: basic_opt (15, string) ^:: basic_opt (16, (message (fun t -> Imported'modules.Descriptor.Google.Protobuf.GeneratedCodeInfo.from_proto_exn t))) ^:: nil ) in
            let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
            fun writer -> deserialize writer
            let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)

        end
        let name' () = "plugin.google.protobuf.compiler.CodeGeneratorResponse"
        type t = { error: string option; supported_features: int option; file: CodeGeneratorResponse.File.t list }
        let make =
          fun ?error ?supported_features ?file () ->
          let file = match file with Some v -> v | None -> [] in
          { error; supported_features; file }

        let to_proto =
          let apply = fun ~f:f' { error; supported_features; file } -> f' [] error supported_features file in
          let spec = Runtime'.Serialize.C.( basic_opt (1, string) ^:: basic_opt (2, uint64_int) ^:: repeated (15, (message (fun t -> CodeGeneratorResponse.File.to_proto t)), not_packed) ^:: nil ) in
          let serialize = Runtime'.Serialize.serialize [] (spec) in
          fun t -> apply ~f:serialize t

        let from_proto_exn =
          let constructor = fun _extensions error supported_features file -> { error; supported_features; file } in
          let spec = Runtime'.Deserialize.C.( basic_opt (1, string) ^:: basic_opt (2, uint64_int) ^:: repeated (15, (message (fun t -> CodeGeneratorResponse.File.from_proto_exn t)), not_packed) ^:: nil ) in
          let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
          fun writer -> deserialize writer
          let from_proto writer = Runtime'.Result.catch (fun () -> from_proto_exn writer)

      end
    end
  end
end