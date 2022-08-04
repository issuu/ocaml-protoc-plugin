let () =
    match Sys.getenv_opt "INCLUDE_GOOGLE_PROTOBUF" with
      Some s -> print_endline s
    | None ->
            let suffix = "google/protobuf" in
            let check_dir prefix =
              let name = Filename.concat prefix suffix in
              if Sys.file_exists name && Sys.is_directory name then
                print_endline prefix;
                true
              else
                false
            in
            if not check_dir "/usr/include" then
              if not check_dir "/usr/local/include" then
                if not check_dir "/opt/homebrew/include" then
                  failwith "Where is the include folder ?"
