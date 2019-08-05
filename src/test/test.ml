module Basic = Basic.Protoc.Plugin.Test

module Simple = Simple.Outer_package.Inner_package

let () =
  let payload = 42 in
  let _basic_message : Basic.Message.t = {payload} in
  let _simple_message : Simple.C.t = { type_1 = payload; type_ = None; type' = None} in
  ()
