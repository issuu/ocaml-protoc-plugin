let () =
    match Sys.getenv_opt "INCLUDE_GOOGLE_PROTOBUF" with
      Some s -> print_endline s
    | None ->
            let suffix = "google/protobuf" in
            let prefix = "/usr/include" in
            let name = Filename.concat prefix suffix in
            if Sys.file_exists name && Sys.is_directory name then
                print_endline name
            else
                let prefix = "/usr/local/include" in
                let name = Filename.concat prefix suffix in
                if Sys.file_exists name && Sys.is_directory name then
                    print_endline name
                else
                    failwith "Where is the include folder ?"
