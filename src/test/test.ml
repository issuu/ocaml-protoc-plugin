open Core

let decode str =
  let filename = "/tmp/test.bin" in
  let cout = Out_channel.create filename in
  Out_channel.output_string cout str;
  Out_channel.close_no_err cout;
  Sys.command_exn (sprintf "protoc --decode_raw < %s" filename)

module Basic = Basic.Protoc.Plugin.Test

let () =
  (*
  let payload = 42 in
  let basic_message : Basic.Message.t = {payload} in
  let (_:string) = Basic.Message.to_proto basic_message in
*)
  let simple_message : Simple.A.t = {a = -7; b = ""} in
  let simple_message_str = Simple.A.to_proto simple_message in
  decode simple_message_str; ()
