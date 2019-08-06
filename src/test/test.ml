module Basic = Basic.Protoc.Plugin.Test

let () =
  let payload = 42 in
  let _basic_message : Basic.Message.t = {payload} in
  let _simple_message : Simple.A.t = {a = 1; b = "5"} in
  ()
