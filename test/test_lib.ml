open Core

module type T = sig
  type t [@@deriving show]
  val to_proto : t -> Protocol.Protobuffer.t
  val from_proto : Protocol.Protobuffer.t -> (t, Protocol.Deserialize.error) result
  val name : string
end

(** Create a common function for testing. *)
let test_encode protobuf_file (type t) (module M : T with type t = t) (t : t) =
  let filename = Filename.temp_file M.name ".bin" in
  let cout = Out_channel.create filename in
  let data = M.to_proto t in
  Out_channel.output_string cout (Protocol.Protobuffer.contents data);
  Out_channel.close_no_err cout;
  Sys.command_exn (sprintf "protoc --decode=%s %s < %s" M.name protobuf_file filename);
  printf "\n";
  (* Decode the message also. I really would like to pass a module. *)
  let result =
    let in_data = Protocol.Protobuffer.reset data in
    (* Protocol.Protobuffer.dump in_data; *)
    match M.from_proto in_data with
    | Ok t' when t' = t -> "Decode success"
    | Ok t' -> sprintf "%s\n   !=\n%s" ([%show: M.t] t) ([%show: M.t] t')
    | Error err -> sprintf "Decode failed: %s" (Protocol.Deserialize.show_error err)
  in
  printf "%s\n" result
