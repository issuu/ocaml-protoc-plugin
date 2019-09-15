open Core

module type T = sig
  type t [@@deriving show]
  val to_proto : t -> Protobuf.Writer.t
  val from_proto : Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) result
  val name : string
end

(** Create a common function for testing. *)
let test_encode protobuf_file (type t) (module M : T with type t = t) (expect : t) =
  let filename = Filename.temp_file M.name ".bin" in
  let cout = Out_channel.create filename in
  let data = M.to_proto expect |> Protobuf.Writer.contents in
  Out_channel.output_string cout data;
  Out_channel.close_no_err cout;
  Sys.command_exn (sprintf "protoc --decode=%s %s < %s | tr \"\\n\" \"; \"" M.name protobuf_file filename);
  Sys.remove filename;
  (* Decode the message *)
  let in_data = Protobuf.Reader.create data in
  (* Protobuf.Protobuffer.dump in_data; *)
  match M.from_proto in_data with
  | Ok observed when expect = observed -> ()
  | Ok observed -> printf "Expect  :%s\nObserved:%s\n" ([%show: M.t] expect) ([%show: M.t] observed)
  | Error err -> printf "Decode failed: %s\n" (Protobuf.Deserialize.show_error err)
