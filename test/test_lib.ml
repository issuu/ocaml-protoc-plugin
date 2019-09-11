open Core

module type T = sig
  type t

  val to_proto : t -> string

  val from_proto : string -> (t, Protocol.Spec.error) result

  val name : string
end

(** Create a common function for testing. *)
let test_encode protobuf_file (type t) (module M : T with type t = t) (t : t) =
  let filename = Filename.temp_file M.name ".bin" in
  let cout = Out_channel.create filename in
  let data = M.to_proto t in
  Out_channel.output_string cout data;
  Out_channel.close_no_err cout;
  Sys.command_exn (sprintf "protoc --decode=%s %s < %s" M.name protobuf_file filename);
  printf "\n";
  (* Decode the message also. I really would like to pass a module. *)
  let result =
    match M.from_proto data with
    | Ok t' when t' = t -> "Decode success"
    | Ok _ -> "Decode mismatch"
    | Error _ -> "Decode failed"
  in
  printf "%s\n" result
