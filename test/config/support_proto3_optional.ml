module C = Configurator.V1

let () =
  C.main ~name:"support_proto3_optionals" (fun t ->
    let r = C.Process.run t "protoc" ["--experimental_allow_proto3_optional"; "--version"] in
    Printf.printf "%b" (r.exit_code = 0)
  )
